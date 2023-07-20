module Page.Login exposing
    ( Memory
    , ScenarioSet
    , init
    , procedure
    , scenario
    , view
    )

{-| Login page.

@docs Memory
@docs ScenarioSet
@docs init
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
import Mixin exposing (Mixin)
import Mixin.Html as Html exposing (Html)
import Page.Login.Login as Login
import Tepa exposing (Layer, Msg, NavKey, Promise, ViewContext)
import Tepa.Http as Http
import Tepa.Navigation as Nav
import Tepa.Random as Random
import Tepa.Scenario as Scenario exposing (Scenario)
import Tepa.Stream as Stream
import Test.Html.Event as HtmlEvent
import Test.Html.Event.Extra as HtmlEvent
import Test.Html.Query as Query
import Test.Html.Selector as Selector
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



-- View


{-| -}
view : Layer Memory -> Html Msg
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


loginFormView : ViewContext LoginFormMemory -> Html Msg
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

        invalidOn : Login.FormError -> Mixin msg
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
                    |> Mixin.fromAttributes
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
                    |> Mixin.fromAttributes
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
            , Mixin.boolAttribute "aria-busy" state.isBusy
            , Mixin.disabled (state.showError && not (List.isEmpty errors))
            , setKey keys.loginFormLoginButton
                |> Mixin.fromAttributes
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
                                        [ Tepa.syncAll
                                            [ Tepa.sequence
                                                [ modifyLoginForm <|
                                                    \m ->
                                                        { m
                                                            | isBusy = False
                                                            , incorrectIdOrPass = True
                                                            , showError = True
                                                        }
                                                , Tepa.lazy <|
                                                    \_ -> loginFormProcedure bucket
                                                ]

                                            -- Remove "IncorrectIdOrPassword" error message when ID or password is changed.
                                            , Tepa.bind
                                                (Stream.firstOfAll
                                                    [ Tepa.viewEventStream
                                                        { key = Login.keys.loginFormId
                                                        , type_ = "change"
                                                        }
                                                    , Tepa.viewEventStream
                                                        { key = Login.keys.loginFormPassword
                                                        , type_ = "change"
                                                        }
                                                    ]
                                                    |> Tepa.void
                                                )
                                              <|
                                                \_ ->
                                                    [ modifyLoginForm <|
                                                        \m -> { m | incorrectIdOrPass = False }
                                                    ]
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
type alias ScenarioSet flags m =
    { layer : Layer m -> Maybe (Layer Memory)
    , changeLoginId :
        { value : String
        }
        -> Scenario.Markup
        -> Scenario flags m
    , changeLoginPass :
        { value : String
        }
        -> Scenario.Markup
        -> Scenario flags m
    , clickSubmitLogin :
        Scenario.Markup -> Scenario flags m
    , receiveLoginResp :
        (Value -> Maybe ( Http.Metadata, String ))
        -> Scenario.Markup
        -> Scenario flags m
    , receiveRandomLuckyHay :
        { value : Session.LuckyHay
        }
        -> Scenario.Markup
        -> Scenario flags m
    , expectAvailable :
        Scenario.Markup -> Scenario flags m
    , expectLoginFormShowNoErrors :
        Scenario.Markup -> Scenario flags m
    , expectLoginFormShowError :
        { error : String
        }
        -> Scenario.Markup
        -> Scenario flags m
    , expectRequestLogin :
        Value -> Scenario.Markup -> Scenario flags m
    , toast : Toast.ScenarioSet flags m
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
scenario : ScenarioProps m -> ScenarioSet flags m
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


changeLoginId : ScenarioProps m -> { value : String } -> Scenario.Markup -> Scenario flags m
changeLoginId props { value } markup =
    Scenario.userOperation props.session
        markup
        { query =
            Query.find
                [ localClassSelector "loginForm_id_input"
                ]
        , operation = HtmlEvent.change value
        }


changeLoginPass : ScenarioProps m -> { value : String } -> Scenario.Markup -> Scenario flags m
changeLoginPass props { value } markup =
    Scenario.userOperation props.session
        markup
        { query =
            Query.find
                [ localClassSelector "loginForm_password_input"
                ]
        , operation = HtmlEvent.change value
        }


clickSubmitLogin : ScenarioProps m -> Scenario.Markup -> Scenario flags m
clickSubmitLogin props markup =
    Scenario.userOperation props.session
        markup
        { query =
            Query.find
                [ localClassSelector "loginForm_loginButton"
                , Selector.disabled False
                ]
        , operation =
            HtmlEvent.click
        }


receiveLoginResp :
    ScenarioProps m
    -> (Value -> Maybe ( Http.Metadata, String ))
    -> Scenario.Markup
    -> Scenario flags m
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
    -> Scenario flags m
receiveRandomLuckyHay props { value } markup =
    Scenario.randomResponse props.session
        markup
        { layer = props.querySelf
        , spec = Session.randomLuckyHay
        , response = value
        }


expectRequestLogin : ScenarioProps m -> Value -> Scenario.Markup -> Scenario flags m
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


expectLoginFormShowNoErrors : ScenarioProps m -> Scenario.Markup -> Scenario flags m
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


expectAvailable : ScenarioProps m -> Scenario.Markup -> Scenario flags m
expectAvailable props markup =
    Scenario.expectMemory props.session
        markup
        { layer = props.querySelf
        , expectation = \_ -> Expect.pass
        }


expectLoginFormShowError : ScenarioProps m -> { error : String } -> Scenario.Markup -> Scenario flags m
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



-- Helper functions


localClass : String -> Mixin msg
localClass name =
    Mixin.class (classPrefix ++ name)


localClassSelector : String -> Selector.Selector
localClassSelector name =
    Selector.class (classPrefix ++ name)


classPrefix : String
classPrefix =
    "page_login--"
