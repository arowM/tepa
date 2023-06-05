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
import App.Path as Path
import App.Session exposing (Session)
import AppUrl exposing (AppUrl)
import Dict
import Json.Encode exposing (Value)
import Mixin.Html as Html exposing (Html)
import Page.Home as PageHome
import Page.Login as PageLogin
import Tepa exposing (Document, Layer, Msg, NavKey, Void)
import Tepa.Http as Http
import Tepa.Navigation as Nav
import Tepa.Scenario as Scenario exposing (Scenario)



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
    , onUrlRequest = onUrlRequest
    , onUrlChange = onUrlChange
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
    = PageLoginEvent PageLogin.Event
    | PageHomeEvent PageHome.Event



-- | PageUsersEvent PageUsers.Event


{-| -}
type alias Promise a =
    Tepa.Promise Memory Event a


type alias Pointer m =
    Tepa.Pointer Memory m



-- -- Initialization


{-| -}
procedure : Value -> AppUrl -> NavKey -> Promise Void
procedure _ url key =
    pageProcedure url key Nothing


onUrlChange : flags -> AppUrl -> NavKey -> Promise Void
onUrlChange _ newUrl key =
    Tepa.bind Tepa.currentState <|
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
                    [ Tepa.bind (Tepa.layerMemory layer) <|
                        \m1 -> [ pageProcedure newUrl key m1.msession ]
                    ]

                PageHome layer ->
                    [ Tepa.bind (Tepa.layerMemory layer) <|
                        \m1 -> [ pageProcedure newUrl key (Just m1.session) ]
                    ]


onUrlRequest : flags -> Tepa.UrlRequest -> NavKey -> Promise Void
onUrlRequest _ urlRequest key =
    case urlRequest of
        Tepa.InternalPath url ->
            Nav.pushPath key url

        Tepa.ExternalPage href ->
            Nav.load href



-- -- Page Controller


pageProcedure :
    AppUrl
    -> NavKey
    -> Maybe Session
    -> Promise Void
pageProcedure url key msession =
    let
        requireSession : Promise Session
        requireSession =
            case msession of
                Just session ->
                    Tepa.succeed session

                Nothing ->
                    -- Check if the user is already logged in.
                    Tepa.bindAndThen FetchProfile.request <|
                        \response ->
                            case response of
                                FetchProfile.GoodResponse resp ->
                                    Tepa.succeed resp

                                _ ->
                                    Nav.redirectPath key
                                        { path =
                                            [ Path.prefix
                                            , "login"
                                            ]
                                        , queryParameters =
                                            Dict.fromList
                                                [ ( "back"
                                                  , [ AppUrl.toString url
                                                    ]
                                                  )
                                                ]
                                        , fragment = Nothing
                                        }
    in
    case Path.body url of
        Just [ "login" ] ->
            -- Users can access login page without sessions.
            Tepa.bind
                (PageLogin.init msession)
            <|
                \initState ->
                    [ Tepa.putVariantLayer
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
                        , init = initState
                        }
                      <|
                        \pointer ->
                            [ PageLogin.procedure key url
                                |> runPageLoginPromise pointer
                            ]
                    ]

        Just [] ->
            Tepa.bind
                (requireSession
                    |> Tepa.andThen PageHome.init
                )
            <|
                \initState ->
                    [ Tepa.putVariantLayer
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
                        , init = initState
                        }
                      <|
                        \pointer ->
                            [ PageHome.procedure key url
                                |> runPageHomePromise pointer
                            ]
                    ]

        _ ->
            Tepa.modify <|
                \m ->
                    { m
                        | page =
                            PageNotFound
                                { msession = msession
                                }
                    }


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
                Scenario.appLayer
                    |> Scenario.childLayer
                        (\m ->
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
                Scenario.appLayer
                    |> Scenario.childLayer
                        (\m ->
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
