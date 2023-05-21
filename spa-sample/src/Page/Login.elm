module Page.Login exposing
    ( Event
    , Memory
    , ScenarioSet
    , init
    , procedure
    , scenario
    , view
    )

{-| Login page.

@docs Event
@docs Memory
@docs ScenarioSet
@docs init
@docs procedure
@docs scenario
@docs view

-}

import App.Route as Route
import App.Session exposing (Session)
import Expect
import Json.Encode exposing (Value)
import Mixin exposing (Mixin)
import Mixin.Events as Events
import Mixin.Html as Html exposing (Html)
import Page.Login.Login as Login
import Tepa exposing (Layer, Msg, NavKey, Void)
import Tepa.AbsolutePath as AbsolutePath
import Tepa.Http as Http
import Tepa.Navigation as Nav
import Tepa.Scenario as Scenario exposing (Scenario)
import Test.Html.Event as HtmlEvent
import Test.Html.Event.Extra as HtmlEvent
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Widget.Toast as Toast


{-| -}
type alias Memory =
    { msession : Maybe Session
    , toast : Maybe (Layer Toast.Memory)
    , loginForm : LoginFormMemory
    }


{-| -}
init : Maybe Session -> Memory
init msession =
    { loginForm = initLoginForm
    , msession = msession
    , toast = Nothing
    }


{-| -}
type Event
    = ToastEvent Toast.Event
    | ChangeLoginId String
    | ChangeLoginPass String
    | ClickSubmitLogin



-- View


{-| -}
view : Layer Memory -> Html (Msg Event)
view =
    Tepa.layerView <|
        \state ->
            Html.div
                [ localClass "page"
                ]
                [ loginFormView state.loginForm
                , case state.toast of
                    Nothing ->
                        Html.text ""

                    Just toast ->
                        Toast.view toast
                            |> Html.map (Tepa.mapMsg ToastEvent)
                ]



-- -- LoginForm


type alias LoginFormMemory =
    { form : Login.Form
    , isBusy : Bool
    , showError : Bool
    , incorrectIdOrPass : Bool
    }


initLoginForm : LoginFormMemory
initLoginForm =
    { form =
        { id = ""
        , pass = ""
        }
    , isBusy = False

    -- Do not show errors initially to avoid bothering
    -- the user with "Input required" errors
    -- when they has not yet entered the information.
    , showError = False
    , incorrectIdOrPass = False
    }


loginFormView : LoginFormMemory -> Html (Msg Event)
loginFormView state =
    let
        errors =
            List.concat
                [ if state.incorrectIdOrPass then
                    [ Login.IncorrectIdOrPassword
                    ]

                  else
                    []
                , Login.toFormErrors state.form
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
                , Mixin.attribute "value" state.form.id
                , Mixin.disabled state.isBusy
                , Events.onChange ChangeLoginId
                    |> Tepa.eventMixin
                , localClass "loginForm_id_input"
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
                , Mixin.attribute "value" state.form.pass
                , Mixin.disabled state.isBusy
                , Events.onChange ChangeLoginPass
                    |> Tepa.eventMixin
                , localClass "loginForm_password_input"
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
            , Events.onClick ClickSubmitLogin
                |> Tepa.eventMixin
            , Mixin.boolAttribute "aria-busy" state.isBusy
            , Mixin.disabled (state.showError && not (List.isEmpty errors))
            ]
            [ Html.text "Login"
            ]
        ]



-- Procedures


type alias Promise a =
    Tepa.Promise Memory Event a


type alias Pointer m =
    Tepa.Pointer Memory m


type alias Bucket =
    { key : NavKey
    , props : Route.LoginProps
    , toastPointer : Pointer Toast.Memory
    }



-- -- Initialization


{-| -}
procedure : Route.LoginProps -> NavKey -> Promise Void
procedure props key =
    -- Initialize Widget
    Tepa.putMaybeLayer
        { get = .toast
        , set = \toast m -> { m | toast = toast }
        , init = Toast.init
        }
    <|
        \toastPointer ->
            let
                bucket =
                    { key = key
                    , props = props
                    , toastPointer = toastPointer
                    }
            in
            -- Main Procedures
            [ Tepa.syncAll
                [ loginFormProcedure bucket
                ]
            ]


loginFormProcedure : Bucket -> Promise Void
loginFormProcedure bucket =
    let
        modifyLoginForm f =
            Tepa.modify <|
                \m -> { m | loginForm = f m.loginForm }
    in
    Tepa.withLayerEvent <|
        \e ->
            case e of
                ChangeLoginId str ->
                    [ modifyLoginForm <|
                        \m ->
                            { m
                                | form =
                                    let
                                        form =
                                            m.form
                                    in
                                    { form | id = str }
                                , incorrectIdOrPass = False
                            }
                    , Tepa.lazy <|
                        \_ -> loginFormProcedure bucket
                    ]

                ChangeLoginPass str ->
                    [ modifyLoginForm <|
                        \m ->
                            { m
                                | form =
                                    let
                                        form =
                                            m.form
                                    in
                                    { form | pass = str }
                                , incorrectIdOrPass = False
                            }
                    , Tepa.lazy <| \_ -> loginFormProcedure bucket
                    ]

                ClickSubmitLogin ->
                    [ Tepa.lazy <|
                        \_ ->
                            submitLoginProcedure bucket
                    ]

                _ ->
                    []


submitLoginProcedure : Bucket -> Promise Void
submitLoginProcedure bucket =
    let
        modifyLoginForm f =
            Tepa.modify <|
                \m ->
                    { m | loginForm = f m.loginForm }
    in
    Tepa.sequence
        [ modifyLoginForm <|
            \m -> { m | isBusy = True }
        , Tepa.bind Tepa.currentState <|
            \curr ->
                case Login.fromForm curr.loginForm.form of
                    Err _ ->
                        [ modifyLoginForm <|
                            \m ->
                                { m
                                    | isBusy = False
                                    , showError = True
                                }
                        , Tepa.lazy <| \_ -> loginFormProcedure bucket
                        ]

                    Ok login ->
                        [ Tepa.bind (Login.request login) <|
                            \response ->
                                case response of
                                    Login.TemporaryErrorResponse ->
                                        [ Tepa.syncAll
                                            [ Toast.pushError
                                                "Network error, please check your network and try again."
                                                |> runToastPromise bucket.toastPointer
                                                |> Tepa.void
                                            , Tepa.sequence
                                                [ modifyLoginForm <|
                                                    \m ->
                                                        { m | isBusy = False }
                                                , Tepa.lazy <|
                                                    \_ ->
                                                        loginFormProcedure bucket
                                                ]
                                            ]
                                        ]

                                    Login.FatalErrorResponse ->
                                        [ Tepa.syncAll
                                            [ Toast.pushError
                                                "Internal error, please contact our support team."
                                                |> runToastPromise bucket.toastPointer
                                                |> Tepa.void
                                            , Tepa.sequence
                                                [ modifyLoginForm <|
                                                    \m ->
                                                        { m | isBusy = False }
                                                , Tepa.lazy <|
                                                    \_ ->
                                                        loginFormProcedure bucket
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
                                                        }
                                                , Tepa.lazy <|
                                                    \_ ->
                                                        loginFormProcedure bucket
                                                ]
                                            ]
                                        ]

                                    Login.GoodResponse resp ->
                                        [ Tepa.modify <|
                                            \m ->
                                                { m
                                                    | msession =
                                                        Just
                                                            { profile = resp.profile
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
                                        , Nav.pushPath bucket.key
                                            (bucket.props.backUrl
                                                |> Maybe.map AbsolutePath.fromUrl
                                                |> Maybe.withDefault (Route.toAbsolutePath Route.Home)
                                            )
                                        ]
                        ]
        ]



-- Toast


runToastPromise :
    Pointer Toast.Memory
    -> Tepa.Promise Toast.Memory Toast.Event a
    -> Promise a
runToastPromise pointer prom =
    Tepa.onLayer pointer prom
        |> Tepa.liftEvent
            { wrap = ToastEvent
            , unwrap =
                \e ->
                    case e of
                        ToastEvent e1 ->
                            Just e1

                        _ ->
                            Nothing
            }



-- Scenario


{-| -}
type alias ScenarioSet flags m e =
    { layer : Layer m -> Maybe (Layer Memory)
    , changeLoginId :
        { value : String
        }
        -> Scenario.Markup
        -> Scenario flags m e
    , changeLoginPass :
        { value : String
        }
        -> Scenario.Markup
        -> Scenario flags m e
    , clickSubmitLogin :
        Scenario.Markup -> Scenario flags m e
    , receiveLoginResp :
        (Value -> Maybe ( Http.Metadata, String ))
        -> Scenario.Markup
        -> Scenario flags m e
    , expectAvailable :
        Scenario.Markup -> Scenario flags m e
    , expectLoginFormShowNoErrors :
        Scenario.Markup -> Scenario flags m e
    , expectLoginFormShowError :
        { error : String
        }
        -> Scenario.Markup
        -> Scenario flags m e
    , expectRequestLogin :
        Value -> Scenario.Markup -> Scenario flags m e
    , toast : Toast.ScenarioSet flags m e
    , loginEndpoint :
        { method : String
        , url : String
        }
    }


type alias ScenarioProps m e =
    { querySelf : Layer m -> Maybe (Layer Memory)
    , wrapEvent : Event -> e
    , session : Scenario.Session
    }


{-| -}
scenario : ScenarioProps m e -> ScenarioSet flags m e
scenario props =
    { layer = props.querySelf
    , changeLoginId = changeLoginId props
    , changeLoginPass = changeLoginPass props
    , clickSubmitLogin = clickSubmitLogin props
    , receiveLoginResp = receiveLoginResp props
    , expectAvailable = expectAvailable props
    , expectRequestLogin = expectRequestLogin props
    , expectLoginFormShowNoErrors = expectLoginFormShowNoErrors props
    , expectLoginFormShowError = expectLoginFormShowError props
    , toast =
        Toast.scenario
            { querySelf =
                props.querySelf
                    |> Scenario.childLayer .toast
            , wrapEvent = ToastEvent >> props.wrapEvent
            , session = props.session
            }
    , loginEndpoint =
        { method = Login.method
        , url = Login.endpointUrl
        }
    }


changeLoginId : ScenarioProps m e -> { value : String } -> Scenario.Markup -> Scenario flags m e
changeLoginId props { value } markup =
    Scenario.userOperation props.session
        markup
        { query =
            Query.find
                [ localClassSelector "loginForm_id_input"
                ]
        , operation = HtmlEvent.change value
        }


changeLoginPass : ScenarioProps m e -> { value : String } -> Scenario.Markup -> Scenario flags m e
changeLoginPass props { value } markup =
    Scenario.userOperation props.session
        markup
        { query =
            Query.find
                [ localClassSelector "loginForm_password_input"
                ]
        , operation = HtmlEvent.change value
        }


clickSubmitLogin : ScenarioProps m e -> Scenario.Markup -> Scenario flags m e
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
    ScenarioProps m e
    -> (Value -> Maybe ( Http.Metadata, String ))
    -> Scenario.Markup
    -> Scenario flags m e
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


expectRequestLogin : ScenarioProps m e -> Value -> Scenario.Markup -> Scenario flags m e
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


expectLoginFormShowNoErrors : ScenarioProps m e -> Scenario.Markup -> Scenario flags m e
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


expectAvailable : ScenarioProps m e -> Scenario.Markup -> Scenario flags m e
expectAvailable props markup =
    Scenario.expectMemory props.session
        markup
        { layer = props.querySelf
        , expectation = \_ -> Expect.pass
        }


expectLoginFormShowError : ScenarioProps m e -> { error : String } -> Scenario.Markup -> Scenario flags m e
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
