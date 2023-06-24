module App exposing
    ( Memory
    , Page(..)
    , ScenarioSet
    , main
    , props
    , scenario
    )

{-| Application main.

@docs Memory
@docs Page
@docs ScenarioSet
@docs main
@docs props
@docs scenario

-}

import App.FetchProfile as FetchProfile
import App.Path as Path
import App.Session as Session exposing (Session)
import AppUrl exposing (AppUrl)
import Dict
import Json.Encode exposing (Value)
import Mixin.Html as Html exposing (Html)
import Page.Home as PageHome
import Page.Login as PageLogin
import Tepa exposing (Document, Layer, Msg, NavKey, Promise, Void)
import Tepa.Http as Http
import Tepa.Navigation as Nav
import Tepa.Random as Random
import Tepa.Scenario as Scenario exposing (Scenario)



-- App


{-| -}
main : Tepa.Program Value Memory
main =
    Tepa.application props


{-| -}
props : Tepa.ApplicationProps Value Memory
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


view : Layer Memory -> Document Msg
view =
    Tepa.layerDocument <|
        \{ state } ->
            { title = "Sample App"
            , body =
                [ case state.page of
                    PageLoading ->
                        pageLoadingView

                    PageNotFound _ ->
                        pageNotFoundView

                    PageLogin pageLogin ->
                        PageLogin.view pageLogin

                    PageHome pageHome ->
                        PageHome.view pageHome
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
-- -- Initialization


{-| -}
procedure : Value -> AppUrl -> NavKey -> Promise Memory Void
procedure _ url key =
    pageProcedure url key Nothing


onUrlChange : flags -> AppUrl -> NavKey -> Promise Memory Void
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
                    [ Tepa.bind (Tepa.layerState layer) <|
                        \m1 -> [ pageProcedure newUrl key m1.msession ]
                    ]

                PageHome layer ->
                    [ Tepa.bind (Tepa.layerState layer) <|
                        \m1 -> [ pageProcedure newUrl key (Just m1.session) ]
                    ]


onUrlRequest : flags -> Tepa.UrlRequest -> NavKey -> Promise Memory Void
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
    -> Promise Memory Void
pageProcedure url key msession =
    let
        withSession : (Session -> Promise Memory Void) -> Promise Memory Void
        withSession action =
            case msession of
                Just session ->
                    action session

                Nothing ->
                    -- Check if the user is already logged in.
                    Tepa.bindAndThen FetchProfile.request <|
                        \response ->
                            case response of
                                FetchProfile.GoodResponse resp ->
                                    Tepa.bind (Random.request Session.randomLuckyHay) <|
                                        \luckyHay ->
                                            [ action
                                                { profile = resp.profile
                                                , luckyHay = luckyHay
                                                }
                                            ]

                                _ ->
                                    Nav.pushPath key
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
                (PageLogin.init msession
                    |> Tepa.andThen Tepa.newLayer
                )
            <|
                \newLayer ->
                    [ Tepa.modify <| \m -> { m | page = PageLogin newLayer }
                    , PageLogin.procedure key url
                        |> Tepa.onLayer
                            { get =
                                \m ->
                                    case m.page of
                                        PageLogin layer ->
                                            Just layer

                                        _ ->
                                            Nothing
                            , set =
                                \layer m ->
                                    { m | page = PageLogin layer }
                            }
                        |> Tepa.void
                    ]

        Just [] ->
            withSession <|
                \session ->
                    Tepa.bind
                        (PageHome.init session
                            |> Tepa.andThen Tepa.newLayer
                        )
                    <|
                        \newLayer ->
                            [ Tepa.modify <| \m -> { m | page = PageHome newLayer }
                            , PageHome.procedure key url
                                |> Tepa.onLayer
                                    { get =
                                        \m ->
                                            case m.page of
                                                PageHome layer ->
                                                    Just layer

                                                _ ->
                                                    Nothing
                                    , set =
                                        \layer m ->
                                            { m | page = PageHome layer }
                                    }
                                |> Tepa.void
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



-- Scenario


{-| -}
type alias ScenarioSet flags =
    { login : PageLogin.ScenarioSet flags Memory
    , home : PageHome.ScenarioSet flags Memory
    , app :
        { receiveProfile :
            (() -> Maybe ( Http.Metadata, String ))
            -> Scenario.Markup
            -> Scenario flags Memory
        , receiveRandomLuckyHay :
            { value : Session.LuckyHay
            }
            -> Scenario.Markup
            -> Scenario flags Memory
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
            , session = session
            }
    , app =
        { receiveProfile = receiveProfile session
        , receiveRandomLuckyHay = receiveRandomLuckyHay session
        , fetchProfileEndpoint =
            { method = FetchProfile.method
            , url = FetchProfile.endpointUrl
            }
        }
    }


receiveProfile : Scenario.Session -> (() -> Maybe ( Http.Metadata, String )) -> Scenario.Markup -> Scenario flags Memory
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


receiveRandomLuckyHay : Scenario.Session -> { value : Session.LuckyHay } -> Scenario.Markup -> Scenario flags Memory
receiveRandomLuckyHay session { value } markup =
    Scenario.randomResponse session
        markup
        { layer = Just
        , spec = Session.randomLuckyHay
        , response = value
        }
