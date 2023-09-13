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
import Page.Chat as PageChat
import Page.Home as PageHome
import Page.Login as PageLogin
import Page.NotFound as PageNotFound
import Tepa exposing (Document, Layer, NavKey, Promise)
import Tepa.Html as Html exposing (Html)
import Tepa.Http as Http
import Tepa.Navigation as Nav
import Tepa.Random as Random
import Tepa.Scenario as Scenario exposing (Scenario)



-- App


{-| -}
main : Tepa.Program Memory
main =
    Tepa.application props


{-| -}
props : Tepa.ApplicationProps Memory
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
    | PageNotFound (Layer PageNotFound.Memory)
    | PageLogin (Layer PageLogin.Memory)
    | PageHome (Layer PageHome.Memory)
    | PageChat (Layer PageChat.Memory)


type alias PageLayer m1 =
    { get : Memory -> Maybe (Layer m1)
    , set : Layer m1 -> Memory -> Memory
    }


pageNotFoundLayer : PageLayer PageNotFound.Memory
pageNotFoundLayer =
    { get =
        \m ->
            case m.page of
                PageNotFound layer ->
                    Just layer

                _ ->
                    Nothing
    , set =
        \layer m ->
            { m | page = PageNotFound layer }
    }


getPageLoginLayer : Memory -> Maybe (Layer PageLogin.Memory)
getPageLoginLayer m =
    case m.page of
        PageLogin layer ->
            Just layer

        _ ->
            Nothing


pageLoginLayer : PageLayer PageLogin.Memory
pageLoginLayer =
    { get = getPageLoginLayer
    , set =
        \layer m ->
            { m | page = PageLogin layer }
    }


getPageHomeLayer : Memory -> Maybe (Layer PageHome.Memory)
getPageHomeLayer m =
    case m.page of
        PageHome layer ->
            Just layer

        _ ->
            Nothing


pageHomeLayer : PageLayer PageHome.Memory
pageHomeLayer =
    { get = getPageHomeLayer
    , set =
        \layer m ->
            { m | page = PageHome layer }
    }


getPageChatLayer : Memory -> Maybe (Layer PageChat.Memory)
getPageChatLayer m =
    case m.page of
        PageChat layer ->
            Just layer

        _ ->
            Nothing


pageChatLayer : PageLayer PageChat.Memory
pageChatLayer =
    { get = getPageChatLayer
    , set =
        \layer m ->
            { m | page = PageChat layer }
    }



-- View


view : Memory -> Document
view state =
    { title = "Sample App"
    , body =
        [ case state.page of
            PageLoading ->
                pageLoadingView

            PageNotFound pageNotFound ->
                PageNotFound.view pageNotFound

            PageLogin pageLogin ->
                PageLogin.view pageLogin

            PageHome pageHome ->
                PageHome.view pageHome

            PageChat pageHome ->
                PageChat.view pageHome
        ]
    }



-- -- PageLoading


pageLoadingView : Html
pageLoadingView =
    Html.text "Loading..."



-- Procedures
-- -- Initialization


{-| -}
procedure : Value -> AppUrl -> NavKey -> Promise Memory ()
procedure _ url key =
    pageProcedure url key Nothing


onUrlChange : Value -> AppUrl -> NavKey -> Promise Memory ()
onUrlChange _ newUrl key =
    Tepa.bindAll
        [ Tepa.bindAndThen Tepa.currentState <|
            \{ page } ->
                if page == PageLoading then
                    Tepa.succeed <| Just Nothing

                else
                    Tepa.succeed Nothing
        , onLeave pageNotFoundLayer PageNotFound.leave
        , onLeave pageLoginLayer PageLogin.leave
        , onLeave pageHomeLayer PageHome.leave
        , onLeave pageChatLayer PageChat.leave
        ]
    <|
        \results ->
            let
                msession =
                    List.filterMap identity results
                        |> List.head
                        |> Maybe.withDefault Nothing
            in
            [ pageProcedure newUrl key msession
            ]


onLeave :
    PageLayer m1
    -> Promise m1 a
    -> Promise Memory (Maybe a)
onLeave pageLayer leave =
    Tepa.onLayer pageLayer leave
        |> Tepa.map
            (\res ->
                case res of
                    Tepa.LayerOk a ->
                        Just a

                    _ ->
                        Nothing
            )


onUrlRequest : Value -> Tepa.UrlRequest -> NavKey -> Promise Memory ()
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
    -> Promise Memory ()
pageProcedure url key msession =
    let
        withSession : (Session -> Promise Memory ()) -> Promise Memory ()
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
                        |> Tepa.onLayer pageLoginLayer
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
                                |> Tepa.onLayer pageHomeLayer
                                |> Tepa.void
                            ]

        Just [ "chat" ] ->
            withSession <|
                \session ->
                    Tepa.bind
                        (PageChat.init session
                            |> Tepa.andThen Tepa.newLayer
                        )
                    <|
                        \newLayer ->
                            [ Tepa.modify <| \m -> { m | page = PageChat newLayer }
                            , PageChat.procedure key url
                                |> Tepa.onLayer pageChatLayer
                                |> Tepa.void
                            ]

        _ ->
            Tepa.bind
                (PageNotFound.init msession
                    |> Tepa.andThen Tepa.newLayer
                )
            <|
                \newLayer ->
                    [ Tepa.modify <| \m -> { m | page = PageNotFound newLayer }
                    , PageNotFound.procedure key url
                        |> Tepa.onLayer pageNotFoundLayer
                        |> Tepa.void
                    ]



-- Scenario


{-| -}
type alias ScenarioSet =
    { login : PageLogin.ScenarioSet Memory
    , home : PageHome.ScenarioSet Memory
    , chat : PageChat.ScenarioSet Memory
    , app :
        { receiveProfile :
            (() -> Maybe ( Http.Metadata, String ))
            -> Scenario.Markup
            -> Scenario Memory
        , receiveRandomLuckyHay :
            { value : Session.LuckyHay
            }
            -> Scenario.Markup
            -> Scenario Memory
        , fetchProfileEndpoint :
            { method : String
            , url : String
            }
        }
    }


{-| -}
scenario : Scenario.Session -> ScenarioSet
scenario session =
    { login =
        PageLogin.scenario
            { querySelf =
                Scenario.appLayer
                    |> Scenario.childLayer getPageLoginLayer
            , session = session
            }
    , home =
        PageHome.scenario
            { querySelf =
                Scenario.appLayer
                    |> Scenario.childLayer getPageHomeLayer
            , session = session
            }
    , chat =
        PageChat.scenario
            { querySelf =
                Scenario.appLayer
                    |> Scenario.childLayer getPageChatLayer
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


receiveProfile : Scenario.Session -> (() -> Maybe ( Http.Metadata, String )) -> Scenario.Markup -> Scenario Memory
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


receiveRandomLuckyHay : Scenario.Session -> { value : Session.LuckyHay } -> Scenario.Markup -> Scenario Memory
receiveRandomLuckyHay session { value } markup =
    Scenario.randomResponse session
        markup
        { layer = Just
        , spec = Session.randomLuckyHay
        , response = value
        }
