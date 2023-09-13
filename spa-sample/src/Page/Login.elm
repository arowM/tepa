module Page.Login exposing
    ( Memory
    , ScenarioSet
    , init
    , leave
    , procedure
    , scenario
    , view
    )

{-| Login page.

@docs Memory
@docs ScenarioSet
@docs init
@docs leave
@docs procedure
@docs scenario
@docs view

-}

import App.Path as Path
import App.Session as Session exposing (Session)
import AppUrl exposing (AppUrl)
import Dict
import Expect
import Json.Encode exposing (Value)
import Page.Login.Login as Login
import Tepa exposing (Layer, NavKey, Promise, ViewContext)
import Tepa.Html as Html exposing (Html)
import Tepa.HtmlSelector as Selector
import Tepa.Http as Http
import Tepa.Mixin as Mixin exposing (Mixin)
import Tepa.Navigation as Nav
import Tepa.Random as Random
import Tepa.Scenario as Scenario exposing (Scenario)
import Tepa.Stream as Stream
import Test.Html.Event as HtmlEvent
import Test.Html.Event.Extra as HtmlEvent
import Test.Html.Query as Query
import Widget.Toast as Toast


{-| -}
type alias Memory =
    { msession : Maybe Session
    , toast : Toast.Memory
    , loginForm : LoginFormMemory
    }


{-| -}
init : Maybe Session -> Promise m Memory
init msession =
    Tepa.succeed
        (\toast ->
            { loginForm = initLoginForm
            , msession = msession
            , toast = toast
            }
        )
        |> Tepa.sync Toast.init


{-| -}
leave : Promise Memory (Maybe Session)
leave =
    Tepa.currentState
        |> Tepa.map .msession



-- View


{-| -}
view : Layer Memory -> Html
view =
    Tepa.layerView <|
        \context ->
            Html.div
                [ localClass "page"
                ]
                [ loginFormView
                    (Tepa.mapViewContext .loginForm context)
                , Toast.view
                    (Tepa.mapViewContext .toast context)
                ]



-- -- LoginForm


type alias LoginFormMemory =
    { isBusy : Bool
    , showError : Bool
    , incorrectIdOrPass : Bool
    }


initLoginForm : LoginFormMemory
initLoginForm =
    { isBusy = False

    -- Do not show errors initially to avoid bothering
    -- the user with "Input required" errors
    -- when they has not yet entered the information.
    , showError = False
    , incorrectIdOrPass = False
    }


loginFormView : ViewContext LoginFormMemory -> Html
loginFormView { state, setKey, values } =
    let
        errors =
            List.concat
                [ if state.incorrectIdOrPass then
                    [ Login.IncorrectIdOrPassword
                    ]

                  else
                    []
                , Login.toFormErrors values
                ]

        invalidOn : Login.FormError -> Mixin
        invalidOn err =
            Mixin.boolAttribute "aria-invalid"
                (state.showError
                    && List.member err errors
                )
    in
    Html.div
        [ localClass "loginForm"
        , Mixin.boolAttribute "aria-invalid"
            (state.showError && not (List.isEmpty errors))
        ]
        [ Html.div
            [ localClass "loginForm_title"
            ]
            [ Html.text "Goat Manager 🐐"
            ]
        , Html.div
            [ localClass "loginForm_notes"
            ]
            [ Html.div
                [ localClass "loginForm_notes_head"
                ]
                [ Html.text "For guests"
                ]
            , Html.div
                [ localClass "loginForm_notes_text"
                ]
                [ Html.text "ID: guest"
                ]
            , Html.div
                [ localClass "loginForm_notes_text"
                ]
                [ Html.text "Password: guestPass"
                ]
            ]
        , Html.node "label"
            [ localClass "loginForm_id"
            , invalidOn Login.IdRequired
            ]
            [ Html.span
                [ localClass "loginForm_id_label"
                ]
                [ Html.text "ID"
                ]
            , Html.node "input"
                [ Mixin.attribute "type" "text"
                , Mixin.disabled state.isBusy
                , localClass "loginForm_id_input"
                , setKey Login.keys.loginFormId
                ]
                []
            ]
        , Html.node "label"
            [ localClass "loginForm_password"
            , invalidOn Login.PassRequired
            ]
            [ Html.span
                [ localClass "loginForm_password_label"
                ]
                [ Html.text "Password"
                ]
            , Html.node "input"
                [ Mixin.attribute "type" "password"
                , Mixin.disabled state.isBusy
                , localClass "loginForm_password_input"
                , setKey Login.keys.loginFormPassword
                ]
                []
            ]
        , if state.showError && List.length errors > 0 then
            Html.div
                [ localClass "loginForm_errorField"
                ]
                (List.map
                    (\err ->
                        Html.div
                            [ localClass "loginForm_errorField_error"
                            ]
                            [ Html.text <| Login.displayFormError err
                            ]
                    )
                    errors
                )

          else
            Html.text ""
        , Html.node "button"
            [ localClass "loginForm_loginButton"
            , Mixin.attribute "type" "button"
            , Mixin.boolAttribute "aria-busy" state.isBusy

            -- We intentionally use the `aria-disabled` attribute here instead of the `disabled` attribute.
            -- If you use the `disabled` attribute, for example, if a user once presses the login button with an incorrect password, then corrects the password again and wants the tab key to focus on the login button, it will not focus properly.
            , Mixin.boolAttribute "aria-disabled" <| state.showError && not (List.isEmpty errors)
            , setKey keys.loginFormLoginButton
            ]
            [ Html.text "Login"
            ]
        ]


keys :
    { loginFormLoginButton : String
    }
keys =
    { loginFormLoginButton = "loginFormLoginButton"
    }



-- Procedures


type alias Bucket =
    { key : NavKey
    , requestPath : AppUrl
    }



-- -- Initialization


{-| -}
procedure : NavKey -> AppUrl -> Promise Memory ()
procedure key url =
    let
        bucket =
            { key = key
            , requestPath = url
            }
    in
    -- Main Procedures
    Tepa.syncAll
        [ loginFormProcedure bucket
        ]


loginFormProcedure : Bucket -> Promise Memory ()
loginFormProcedure bucket =
    -- IGNORE TCO
    let
        modifyLoginForm f =
            Tepa.modify <|
                \m -> { m | loginForm = f m.loginForm }
    in
    Tepa.sequence
        [ Tepa.awaitViewEvent
            { key = keys.loginFormLoginButton
            , type_ = "click"
            }
        , modifyLoginForm <|
            \m -> { m | isBusy = True }
        , Tepa.bind Tepa.getValues <|
            \form ->
                case Login.fromForm form of
                    Err _ ->
                        [ modifyLoginForm <|
                            \m ->
                                { m
                                    | isBusy = False
                                    , showError = True
                                }
                        , Tepa.lazy <|
                            \_ -> loginFormProcedure bucket
                        ]

                    Ok login ->
                        [ Tepa.bind (Login.request login) <|
                            \response ->
                                case response of
                                    Login.TemporaryErrorResponse ->
                                        [ Tepa.syncAll
                                            [ Toast.pushError
                                                "Network error, please check your network and try again."
                                                |> runToastPromise
                                                |> Tepa.void
                                            , Tepa.sequence
                                                [ modifyLoginForm <|
                                                    \m ->
                                                        { m | isBusy = False }
                                                , Tepa.lazy <|
                                                    \_ -> loginFormProcedure bucket
                                                ]
                                            ]
                                        ]

                                    Login.FatalErrorResponse ->
                                        [ Tepa.syncAll
                                            [ Toast.pushError
                                                "Internal error, please contact our support team."
                                                |> runToastPromise
                                                |> Tepa.void
                                            , Tepa.sequence
                                                [ modifyLoginForm <|
                                                    \m ->
                                                        { m | isBusy = False }
                                                , Tepa.lazy <|
                                                    \_ -> loginFormProcedure bucket
                                                ]
                                            ]
                                        ]

                                    Login.IncorrectIdOrPasswordResponse ->
                                        [ modifyLoginForm <|
                                            \m ->
                                                { m
                                                    | isBusy = False
                                                    , incorrectIdOrPass = True
                                                    , showError = True
                                                }

                                        -- Remove "IncorrectIdOrPassword" error message when ID or password is changed.
                                        , Tepa.bindAll
                                            [ Tepa.viewEventStream
                                                { key = Login.keys.loginFormId
                                                , type_ = "change"
                                                }
                                            , Tepa.viewEventStream
                                                { key = Login.keys.loginFormPassword
                                                , type_ = "change"
                                                }
                                            ]
                                          <|
                                            \streams ->
                                                [ Stream.awaitFirst (Stream.union streams)
                                                    |> Tepa.void
                                                , modifyLoginForm <|
                                                    \m -> { m | incorrectIdOrPass = False }
                                                , Tepa.lazy <|
                                                    \_ -> loginFormProcedure bucket
                                                ]
                                        ]

                                    Login.GoodResponse resp ->
                                        [ Tepa.bind (Random.request Session.randomLuckyHay) <|
                                            \luckyHay ->
                                                [ Tepa.modify <|
                                                    \m ->
                                                        { m
                                                            | msession =
                                                                Just
                                                                    { profile = resp.profile
                                                                    , luckyHay = luckyHay
                                                                    }
                                                            , loginForm =
                                                                let
                                                                    loginForm =
                                                                        m.loginForm
                                                                in
                                                                { loginForm
                                                                    | isBusy = False
                                                                }
                                                        }
                                                ]
                                        , Nav.pushPath bucket.key
                                            (bucket.requestPath.queryParameters
                                                |> Dict.get "back"
                                                |> Maybe.andThen List.head
                                                |> Maybe.andThen Path.toAppUrl
                                                |> Maybe.withDefault
                                                    { path =
                                                        [ Path.prefix
                                                        ]
                                                    , queryParameters = Dict.empty
                                                    , fragment = Nothing
                                                    }
                                            )
                                        ]
                        ]
        ]



-- Toast


runToastPromise :
    Promise Toast.Memory a
    -> Promise Memory a
runToastPromise prom =
    Tepa.liftMemory
        { get = .toast
        , set = \toast m -> { m | toast = toast }
        }
        prom



-- Scenario


{-| -}
type alias ScenarioSet m =
    { layer : Layer m -> Maybe (Layer Memory)
    , changeLoginId :
        { value : String
        }
        -> Scenario.Markup
        -> Scenario m
    , changeLoginPass :
        { value : String
        }
        -> Scenario.Markup
        -> Scenario m
    , clickSubmitLogin :
        Scenario.Markup -> Scenario m
    , receiveLoginResp :
        (Value -> Maybe ( Http.Metadata, String ))
        -> Scenario.Markup
        -> Scenario m
    , receiveRandomLuckyHay :
        { value : Session.LuckyHay
        }
        -> Scenario.Markup
        -> Scenario m
    , expectAvailable :
        Scenario.Markup -> Scenario m
    , expectLoginFormShowNoErrors :
        Scenario.Markup -> Scenario m
    , expectLoginFormShowError :
        { error : String
        }
        -> Scenario.Markup
        -> Scenario m
    , expectLoginButtonIsBusy :
        Bool -> Scenario.Markup -> Scenario m
    , expectRequestLogin :
        Value -> Scenario.Markup -> Scenario m
    , toast : Toast.ScenarioSet m
    , loginEndpoint :
        { method : String
        , url : String
        }
    }


type alias ScenarioProps m =
    { querySelf : Layer m -> Maybe (Layer Memory)
    , session : Scenario.Session
    }


{-| -}
scenario : ScenarioProps m -> ScenarioSet m
scenario props =
    { layer = props.querySelf
    , changeLoginId = changeLoginId props
    , changeLoginPass = changeLoginPass props
    , clickSubmitLogin = clickSubmitLogin props
    , receiveLoginResp = receiveLoginResp props
    , receiveRandomLuckyHay = receiveRandomLuckyHay props
    , expectAvailable = expectAvailable props
    , expectRequestLogin = expectRequestLogin props
    , expectLoginFormShowNoErrors = expectLoginFormShowNoErrors props
    , expectLoginFormShowError = expectLoginFormShowError props
    , expectLoginButtonIsBusy = expectLoginButtonIsBusy props
    , toast =
        Toast.scenario
            { querySelf =
                props.querySelf
                    |> Scenario.mapLayer .toast
            , session = props.session
            }
    , loginEndpoint =
        { method = Login.method
        , url = Login.endpointUrl
        }
    }


changeLoginId : ScenarioProps m -> { value : String } -> Scenario.Markup -> Scenario m
changeLoginId props { value } markup =
    Scenario.userOperation props.session
        markup
        { query =
            Query.find
                [ localClassSelector "loginForm_id_input"
                ]
        , operation = HtmlEvent.change value
        }


changeLoginPass : ScenarioProps m -> { value : String } -> Scenario.Markup -> Scenario m
changeLoginPass props { value } markup =
    Scenario.userOperation props.session
        markup
        { query =
            Query.find
                [ localClassSelector "loginForm_password_input"
                ]
        , operation = HtmlEvent.change value
        }


clickSubmitLogin : ScenarioProps m -> Scenario.Markup -> Scenario m
clickSubmitLogin props markup =
    Scenario.userOperation props.session
        markup
        { query =
            Query.find
                [ localClassSelector "loginForm_loginButton"
                , Selector.attribute "aria-disabled" "false"
                , Selector.attribute "aria-busy" "false"
                ]
        , operation =
            HtmlEvent.click
        }


receiveLoginResp :
    ScenarioProps m
    -> (Value -> Maybe ( Http.Metadata, String ))
    -> Scenario.Markup
    -> Scenario m
receiveLoginResp props toResponse markup =
    Scenario.httpResponse props.session
        markup
        { layer = props.querySelf
        , response =
            \rawRequest ->
                if rawRequest.url == Login.endpointUrl && rawRequest.method == Login.method then
                    case rawRequest.requestBody of
                        Scenario.JsonHttpRequestBody value ->
                            toResponse value

                        _ ->
                            Nothing

                else
                    Nothing
        }


receiveRandomLuckyHay :
    ScenarioProps m
    -> { value : Session.LuckyHay }
    -> Scenario.Markup
    -> Scenario m
receiveRandomLuckyHay props { value } markup =
    Scenario.randomResponse props.session
        markup
        { layer = props.querySelf
        , spec = Session.randomLuckyHay
        , response = value
        }


expectRequestLogin : ScenarioProps m -> Value -> Scenario.Markup -> Scenario m
expectRequestLogin props requestBody markup =
    Scenario.expectHttpRequest props.session
        markup
        { layer =
            props.querySelf
        , expectation =
            List.filter
                (\request ->
                    request.method
                        == Login.method
                        && request.url
                        == Login.endpointUrl
                        && request.requestBody
                        == Scenario.JsonHttpRequestBody requestBody
                )
                >> List.length
                >> Expect.greaterThan 0
        }


expectLoginFormShowNoErrors : ScenarioProps m -> Scenario.Markup -> Scenario m
expectLoginFormShowNoErrors props markup =
    Scenario.expectAppView props.session
        markup
        { expectation =
            \{ body } ->
                Query.fromHtml (Html.div [] body)
                    |> Query.find
                        [ localClassSelector "loginForm"
                        ]
                    |> Query.findAll
                        [ localClassSelector "loginForm_errorField_error"
                        ]
                    |> Query.count (Expect.equal 0)
        }


expectAvailable : ScenarioProps m -> Scenario.Markup -> Scenario m
expectAvailable props markup =
    Scenario.expectMemory props.session
        markup
        { layer = props.querySelf
        , expectation = \_ -> Expect.pass
        }


expectLoginFormShowError : ScenarioProps m -> { error : String } -> Scenario.Markup -> Scenario m
expectLoginFormShowError props { error } markup =
    Scenario.expectAppView props.session
        markup
        { expectation =
            \{ body } ->
                Query.fromHtml (Html.div [] body)
                    |> Query.find
                        [ localClassSelector "loginForm"
                        ]
                    |> Query.findAll
                        [ localClassSelector "loginForm_errorField_error"
                        , Selector.text error
                        ]
                    |> Query.count (Expect.greaterThan 0)
        }


expectLoginButtonIsBusy :
    ScenarioProps m
    -> Bool
    -> Scenario.Markup
    -> Scenario m
expectLoginButtonIsBusy props isBusy markup =
    Scenario.expectAppView props.session
        markup
        { expectation =
            \{ body } ->
                Query.fromHtml (Html.div [] body)
                    |> Query.find
                        [ localClassSelector "loginForm_loginButton"
                        ]
                    |> Query.has
                        [ Selector.boolAttribute "aria-busy" isBusy
                        ]
        }



-- Helper functions


localClass : String -> Mixin
localClass name =
    Mixin.class (pagePrefix ++ name)


localClassSelector : String -> Selector.Selector
localClassSelector name =
    Selector.class (pagePrefix ++ name)


pagePrefix : String
pagePrefix =
    "page_login--"
