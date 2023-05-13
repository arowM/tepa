module App exposing
    ( Event
    , Memory
    , Page(..)
    , Promise
    , ScenarioSet
    , main
    , props
    , scenario
    )

{-| Application main.

@docs Event
@docs Memory
@docs Page
@docs Promise
@docs ScenarioSet
@docs main
@docs props
@docs scenario

-}

import App.FetchProfile as FetchProfile
import App.Route as Route
import App.Session exposing (Session)
import Browser exposing (Document)
import Json.Encode exposing (Value)
import Mixin.Html as Html exposing (Html)
import Page.Home as PageHome
import Page.Login as PageLogin
import Tepa exposing (Layer, Msg, NavKey, Void)
import Tepa.AbsolutePath as AbsolutePath
import Tepa.Http as Http
import Tepa.Navigation as Nav
import Tepa.Scenario as Scenario exposing (Scenario)
import Url exposing (Url)



-- App


{-| -}
main : Tepa.Program Value Memory Event
main =
    Tepa.application props


{-| -}
props : Tepa.ApplicationProps Value Memory Event
props =
    { init = init
    , procedure = procedure
    , view = view
    , onUrlRequest = LinkClicked
    , onUrlChange = UrlChanged
    }



-- Memory


{-| -}
type alias Memory =
    { page : Page
    }


init : Memory
init =
    { page = PageLoading
    }



-- Page


{-| -}
type Page
    = PageLoading
    | PageNotFound
        { msession : Maybe Session
        }
    | PageLogin (Layer PageLogin.Memory)
    | PageHome (Layer PageHome.Memory)



-- View


view : Layer Memory -> Document (Msg Event)
view =
    Tepa.layerDocument <|
        \memory ->
            { title = "Sample App"
            , body =
                [ case memory.page of
                    PageLoading ->
                        pageLoadingView

                    PageNotFound _ ->
                        pageNotFoundView

                    PageLogin pageLogin ->
                        PageLogin.view pageLogin
                            |> Html.map (Tepa.mapMsg PageLoginEvent)

                    PageHome pageHome ->
                        PageHome.view pageHome
                            |> Html.map (Tepa.mapMsg PageHomeEvent)
                ]
            }



-- -- PageLoading


pageLoadingView : Html msg
pageLoadingView =
    Html.text "Loading..."



-- -- PageNotFound


pageNotFoundView : Html msg
pageNotFoundView =
    Html.text "Not found"



-- Procedures


{-| -}
type Event
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | PageLoginEvent PageLogin.Event
    | PageHomeEvent PageHome.Event



-- | PageUsersEvent PageUsers.Event


{-| -}
type alias Promise a =
    Tepa.Promise Memory Event a


type alias Pointer m =
    Tepa.Pointer Memory m



-- -- Initialization


{-| -}
procedure : Value -> Url -> NavKey -> Promise Void
procedure _ url key =
    Tepa.syncAll
        -- Monitor app Events.
        [ Tepa.listenLayerEvent (appEventHandler key)

        -- Process initial procedures on loading app.
        , pageProcedure url key Nothing
        ]


appEventHandler : NavKey -> Event -> List (Promise Void)
appEventHandler key event =
    case event of
        UrlChanged newUrl ->
            [ Tepa.bind Tepa.currentState <|
                \state ->
                    case state.page of
                        PageLoading ->
                            [ Tepa.lazy <|
                                \_ -> pageProcedure newUrl key Nothing
                            ]

                        PageNotFound param ->
                            [ Tepa.lazy <|
                                \_ -> pageProcedure newUrl key param.msession
                            ]

                        PageLogin layer ->
                            [ Tepa.lazy <|
                                \_ -> pageProcedure newUrl key (Tepa.layerMemory layer).msession
                            ]

                        PageHome layer ->
                            [ Tepa.lazy <|
                                \_ -> pageProcedure newUrl key (Just (Tepa.layerMemory layer).session)
                            ]
            ]

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    [ Nav.pushPath key (AbsolutePath.fromUrl url)
                    ]

                Browser.External href ->
                    [ Nav.load href
                    ]

        _ ->
            []



-- -- Page Controller


pageProcedure :
    Url
    -> NavKey
    -> Maybe Session
    -> Promise Void
pageProcedure url key msession =
    case ( Route.fromUrl url, msession ) of
        ( Route.NotFound, _ ) ->
            Tepa.modify <|
                \m ->
                    { m
                        | page =
                            PageNotFound
                                { msession = msession
                                }
                    }

        -- Users can access login page without sessions.
        ( Route.Login login, _ ) ->
            Tepa.putVariantLayer
                { get = .page
                , set = \v m -> { m | page = v }
                , wrap = PageLogin
                , unwrap =
                    \m ->
                        case m of
                            PageLogin layer ->
                                Just layer

                            _ ->
                                Nothing
                , init = PageLogin.init msession
                }
            <|
                \pointer ->
                    [ PageLogin.procedure login key
                        |> runPageLoginPromise pointer
                    ]

        ( _, Nothing ) ->
            -- Check if the user has already logged in.
            Tepa.bind FetchProfile.request <|
                \response ->
                    case response of
                        FetchProfile.GoodResponse resp ->
                            [ Tepa.lazy <|
                                \_ ->
                                    pageProcedure url
                                        key
                                        (Just
                                            { profile = resp.profile
                                            }
                                        )
                            ]

                        _ ->
                            [ Nav.pushPath key
                                (Route.Login { backUrl = Just url }
                                    |> Route.toAbsolutePath
                                )
                            ]

        ( Route.Home, Just session ) ->
            Tepa.putVariantLayer
                { get = .page
                , set = \a m -> { m | page = a }
                , wrap = PageHome
                , unwrap =
                    \m ->
                        case m of
                            PageHome a ->
                                Just a

                            _ ->
                                Nothing
                , init = PageHome.init session
                }
            <|
                \pointer ->
                    [ PageHome.procedure key url
                        |> runPageHomePromise pointer
                    ]

        _ ->
            Tepa.none


runPageLoginPromise :
    Pointer PageLogin.Memory
    -> Tepa.Promise PageLogin.Memory PageLogin.Event a
    -> Promise a
runPageLoginPromise pointer prom =
    Tepa.onLayer pointer prom
        |> Tepa.liftEvent
            { wrap = PageLoginEvent
            , unwrap =
                \e ->
                    case e of
                        PageLoginEvent e1 ->
                            Just e1

                        _ ->
                            Nothing
            }


runPageHomePromise :
    Pointer PageHome.Memory
    -> Tepa.Promise PageHome.Memory PageHome.Event a
    -> Promise a
runPageHomePromise pointer prom =
    Tepa.onLayer pointer prom
        |> Tepa.liftEvent
            { wrap = PageHomeEvent
            , unwrap =
                \e ->
                    case e of
                        PageHomeEvent e1 ->
                            Just e1

                        _ ->
                            Nothing
            }



-- Scenario


{-| -}
type alias ScenarioSet flags =
    { login : PageLogin.ScenarioSet flags Memory Event
    , home : PageHome.ScenarioSet flags Memory Event
    , app :
        { receiveProfile :
            (() -> Maybe ( Http.Metadata, String ))
            -> Scenario.Markup
            -> Scenario flags Memory Event
        , fetchProfileEndpoint :
            { method : String
            , url : String
            }
        }
    }


{-| -}
scenario : Scenario.Session -> ScenarioSet flags
scenario session =
    { login =
        PageLogin.scenario
            { querySelf =
                Tepa.layerMemory
                    >> (\m ->
                            case m.page of
                                PageLogin l ->
                                    Just l

                                _ ->
                                    Nothing
                       )
            , wrapEvent = PageLoginEvent
            , session = session
            }
    , home =
        PageHome.scenario
            { querySelf =
                Tepa.layerMemory
                    >> (\m ->
                            case m.page of
                                PageHome l ->
                                    Just l

                                _ ->
                                    Nothing
                       )
            , wrapEvent = PageHomeEvent
            , session = session
            }
    , app =
        { receiveProfile = receiveProfile session
        , fetchProfileEndpoint =
            { method = FetchProfile.method
            , url = FetchProfile.endpointUrl
            }
        }
    }


receiveProfile : Scenario.Session -> (() -> Maybe ( Http.Metadata, String )) -> Scenario.Markup -> Scenario flags Memory Event
receiveProfile session toResponse markup =
    Scenario.httpResponse session
        markup
        { layer = Just
        , response =
            \rawRequest ->
                if rawRequest.url == FetchProfile.endpointUrl then
                    toResponse ()

                else
                    Nothing
        }
