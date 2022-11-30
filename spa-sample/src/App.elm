module App exposing
    ( Command(..)
    , Event
    , Memory
    , Page(..)
    , Promise
    , ScenarioSet
    , main
    , props
    , scenario
    )

-- import Page.Users as PageUsers

import App.FetchProfile as FetchProfile
import App.Route as Route
import App.Session exposing (Session)
import Browser exposing (Document)
import Browser.Navigation
import Json.Encode exposing (Value)
import Mixin.Html as Html exposing (Html)
import Page.Home as PageHome
import Page.Login as PageLogin
import Tepa exposing (Layer, Msg, Void)
import Tepa.AbsolutePath as AbsolutePath
import Tepa.Navigation as Nav exposing (NavKey)
import Tepa.ResponseType as ResponseType
import Tepa.Scenario as Scenario exposing (Scenario)
import Tepa.Scenario.LayerQuery as LayerQuery
import Url exposing (Url)



-- App


{-| -}
main : Tepa.Program Value Memory Event
main =
    Tepa.application
        { props = props
        , runCommand = runCommand
        }


{-| -}
props : Tepa.ApplicationProps Value Command Memory Event
props =
    { init = init
    , procedure = procedure
    , view = view
    , onUrlRequest = LinkClicked
    , onUrlChange = UrlChanged
    }



-- Memory


type alias Memory =
    { page : Page
    }


init : Memory
init =
    { page = PageLoading
    }



-- Page


type Page
    = PageLoading
    | PageNotFound
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

                    PageNotFound ->
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


{-| Abstructed Commands, which enables dependency injection.
-}
type Command
    = PageLoginCommand PageLogin.Command
    | PageHomeCommand PageHome.Command
      -- | PageUsersCommand PageUsers.Command
    | FetchProfile (Tepa.HttpResult String -> Msg Event)
    | LoadPage String


{-| Run abstructed Commands as actual application Commands.
-}
runCommand : Command -> Cmd (Msg Event)
runCommand cmd =
    case cmd of
        PageLoginCommand c ->
            PageLogin.runCommand c
                |> Cmd.map (Tepa.mapMsg PageLoginEvent)

        PageHomeCommand c ->
            PageHome.runCommand c
                |> Cmd.map (Tepa.mapMsg PageHomeEvent)

        FetchProfile toMsg ->
            FetchProfile.request toMsg

        LoadPage url ->
            Browser.Navigation.load url


{-| -}
type alias Promise a =
    Tepa.Promise Command Memory Event a


type alias Pointer m =
    Tepa.Pointer Memory m



-- -- Initialization


{-| -}
procedure : Value -> Url -> NavKey -> Promise Void
procedure _ url key =
    Tepa.syncAll
        [ linkControllProcedure key
        , pageControllProcedure url key Nothing
        ]



-- -- Link Controller


{-| Handle link-click events.
-}
linkControllProcedure : NavKey -> Promise Void
linkControllProcedure key =
    Tepa.withLayerEvent <|
        \e ->
            case e of
                LinkClicked urlRequest ->
                    case urlRequest of
                        Browser.Internal url ->
                            [ Nav.pushPath key (AbsolutePath.fromUrl url)
                            , Tepa.lazy <|
                                \_ -> linkControllProcedure key
                            ]

                        Browser.External href ->
                            [ loadPage href
                            , Tepa.lazy <|
                                \_ -> linkControllProcedure key
                            ]

                _ ->
                    -- Await again when receive other events
                    []


loadPage : String -> Promise Void
loadPage url =
    Tepa.push <| \_ -> LoadPage url



-- -- Page Controller


pageControllProcedure :
    Url
    -> NavKey
    -> Maybe Session
    -> Promise Void
pageControllProcedure url key msession =
    case ( Route.fromUrl url, msession ) of
        ( Route.NotFound, _ ) ->
            Tepa.sequence
                [ Tepa.modify <|
                    \m ->
                        { m | page = PageNotFound }
                , Tepa.withLayerEvent <|
                    \e ->
                        case e of
                            UrlChanged newUrl ->
                                [ Tepa.lazy <|
                                    \_ ->
                                        pageControllProcedure newUrl key msession
                                ]

                            _ ->
                                []
                ]

        ( Route.Login login, _ ) ->
            Tepa.putVariantLayer
                { get = .page
                , set = \v m -> { m | page = v }
                , wrap = PageLogin
                , unwrap =
                    \m ->
                        case m of
                            PageLogin a ->
                                Just a

                            _ ->
                                Nothing
                , init = PageLogin.init
                }
                |> Tepa.andThen
                    (\pageLoginPointer ->
                        Tepa.syncAll
                            -- When login succeed, `PageLogin.procedure` issues `Nav.pushPath` request.
                            [ PageLogin.procedure login key
                                |> runPageLoginPromise pageLoginPointer

                            -- When users do not log in and move to another page:
                            , Tepa.withLayerEvent <|
                                \e2 ->
                                    case e2 of
                                        UrlChanged newUrl ->
                                            [ runPageLoginPromise pageLoginPointer PageLogin.currentSession
                                                |> Tepa.andThen (pageControllProcedure newUrl key)
                                            ]

                                        _ ->
                                            []
                            ]
                    )

        ( _, Nothing ) ->
            requestFetchProfile
                |> Tepa.andThen
                    (\response ->
                        case response of
                            Err _ ->
                                Tepa.sequence
                                    [ Nav.pushPath key
                                        (Route.Login { backUrl = Just url }
                                            |> Route.toAbsolutePath
                                        )
                                    , Tepa.withLayerEvent <|
                                        \e ->
                                            case e of
                                                UrlChanged newUrl ->
                                                    [ pageControllProcedure newUrl key Nothing
                                                    ]

                                                _ ->
                                                    []
                                    ]

                            Ok FetchProfile.LoginRequired ->
                                Tepa.sequence
                                    [ Nav.pushPath key
                                        (Route.Login { backUrl = Just url }
                                            |> Route.toAbsolutePath
                                        )
                                    , Tepa.withLayerEvent <|
                                        \e ->
                                            case e of
                                                UrlChanged newUrl ->
                                                    [ pageControllProcedure newUrl key Nothing
                                                    ]

                                                _ ->
                                                    []
                                    ]

                            Ok FetchProfile.OtherError ->
                                Tepa.sequence
                                    [ Nav.pushPath key
                                        (Route.Login { backUrl = Just url }
                                            |> Route.toAbsolutePath
                                        )
                                    , Tepa.withLayerEvent <|
                                        \e ->
                                            case e of
                                                UrlChanged newUrl ->
                                                    [ pageControllProcedure newUrl key Nothing
                                                    ]

                                                _ ->
                                                    []
                                    ]

                            Ok (FetchProfile.GoodResponse resp) ->
                                Tepa.lazy <|
                                    \_ ->
                                        pageControllProcedure url key (Just { id = resp.id })
                    )

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
                |> Tepa.andThen
                    (\pageHomePointer ->
                        Tepa.syncAll
                            [ PageHome.procedure key url
                                |> runPageHomePromise pageHomePointer
                            , Tepa.withLayerEvent <|
                                \e ->
                                    case e of
                                        UrlChanged newUrl ->
                                            [ runPageHomePromise pageHomePointer PageHome.currentSession
                                                |> Tepa.andThen
                                                    (pageControllProcedure newUrl key << Just)
                                            ]

                                        _ ->
                                            []
                            ]
                    )

        _ ->
            Tepa.none


requestFetchProfile : Promise (Result Tepa.HttpRequestError FetchProfile.Response)
requestFetchProfile =
    Tepa.httpRequest
        { name = "requestFetchProfile"
        , bodyType = ResponseType.string
        , request = FetchProfile
        , response = FetchProfile.response
        }


runPageLoginPromise :
    Pointer PageLogin.Memory
    -> Tepa.Promise PageLogin.Command PageLogin.Memory PageLogin.Event a
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
        |> Tepa.mapCmd PageLoginCommand


runPageHomePromise :
    Pointer PageHome.Memory
    -> Tepa.Promise PageHome.Command PageHome.Memory PageHome.Event a
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
        |> Tepa.mapCmd PageHomeCommand



-- Scenario


{-| -}
type alias ScenarioSet flags =
    { login : PageLogin.ScenarioSet flags Command Memory Event
    , home : PageHome.ScenarioSet flags Command Memory Event
    , app :
        { receiveProfile : Tepa.HttpResult String -> Scenario flags Command Memory Event
        }
    }


{-| -}
scenario : Scenario.Session -> ScenarioSet flags
scenario session =
    { login =
        PageLogin.scenario
            { querySelf =
                LayerQuery.self
                    |> LayerQuery.child
                        (\m ->
                            case m.page of
                                PageLogin l ->
                                    Just l

                                _ ->
                                    Nothing
                        )
            , wrapEvent = PageLoginEvent
            , unwrapCommand =
                \c ->
                    case c of
                        PageLoginCommand c1 ->
                            Just c1

                        _ ->
                            Nothing
            , session = session
            }
    , home =
        PageHome.scenario
            { querySelf =
                LayerQuery.self
                    |> LayerQuery.child
                        (\m ->
                            case m.page of
                                PageHome l ->
                                    Just l

                                _ ->
                                    Nothing
                        )
            , wrapEvent = PageHomeEvent
            , unwrapCommand =
                \c ->
                    case c of
                        PageHomeCommand c1 ->
                            Just c1

                        _ ->
                            Nothing
            , session = session
            }
    , app =
        { receiveProfile = receiveProfile session
        }
    }


receiveProfile : Scenario.Session -> Tepa.HttpResult String -> Scenario flags Command Memory Event
receiveProfile session res =
    Scenario.customResponse session
        "Backend responds to the session fetch request."
        { target = LayerQuery.self
        , response =
            \cmd ->
                case cmd of
                    FetchProfile toMsg ->
                        Just <| toMsg res

                    _ ->
                        Nothing
        }
