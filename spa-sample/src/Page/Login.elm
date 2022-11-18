module Page.Login exposing
    ( Command
    , Event
    , Memory
    , ScenarioSet
    , currentSession
    , init
    , procedure
    , runCommand
    , scenario
    , view
    )

import App.Session exposing (Session)
import Expect
import Expect.Builder
import Http
import Json.Encode exposing (Value)
import Mixin exposing (Mixin)
import Mixin.Events as Events
import Mixin.Html as Html exposing (Html)
import Page.Login.Login as Login
import Tepa exposing (Layer, Msg, Void)
import Tepa.Navigation as Nav exposing (NavKey)
import Tepa.Scenario as Scenario exposing (Scenario)
import Tepa.Scenario.LayerQuery as LayerQuery exposing (LayerQuery)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Url exposing (Url)
import Widget.Toast as Toast


{-| -}
type alias Memory =
    { msession : Maybe Session
    , toast : Maybe (Layer Toast.Memory)
    , loginForm : LoginFormMemory
    }


{-| -}
init : Memory
init =
    { loginForm = initLoginForm
    , msession = Nothing
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
        \memory ->
            Html.div
                [ localClass "page"
                ]
                [ loginFormView memory.loginForm
                , case memory.toast of
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
    }


initLoginForm : LoginFormMemory
initLoginForm =
    { form = Login.initForm
    , isBusy = False

    -- Do not show errors initially to avoid bothering
    -- the user with "Input required" errors
    -- when they has not yet entered the information.
    , showError = False
    }


loginFormView : LoginFormMemory -> Html (Msg Event)
loginFormView memory =
    let
        errors =
            Login.toFormErrors memory.form
    in
    Html.div
        [ localClass "loginForm"
        , Mixin.boolAttribute "aria-invalid"
            (memory.showError && not (List.isEmpty errors))
        ]
        [ Html.node "label"
            [ localClass "loginForm_label-id"
            ]
            [ Html.text "ID:"
            , Html.node "input"
                [ Mixin.attribute "type" "text"
                , Mixin.attribute "value" memory.form.id
                , Mixin.boolAttribute "disabled" memory.isBusy
                , Events.onChange ChangeLoginId
                    |> Tepa.eventMixin
                ]
                []
            ]
        , Html.node "label"
            [ localClass "loginForm_label-password"
            ]
            [ Html.text "Password:"
            , Html.node "input"
                [ Mixin.attribute "type" "password"
                , Mixin.attribute "value" memory.form.pass
                , Mixin.boolAttribute "disabled" memory.isBusy
                , Events.onChange ChangeLoginPass
                    |> Tepa.eventMixin
                ]
                []
            ]
        , Html.node "button"
            [ localClass "loginForm_submitLogin"
            , Events.onClick ClickSubmitLogin
                |> Tepa.eventMixin
            , Mixin.boolAttribute "disabled" memory.isBusy
            ]
            [ Html.text "Login"
            ]
        , Html.div
            [ localClass "loginForm_notes"
            ]
            [ Html.div
                [ localClass "loginForm_notes_head"
                ]
                [ Html.text "For guests:"
                ]
            , Html.div
                [ localClass "loginForm_notes_text"
                ]
                [ Html.text "ID: guest"
                ]
            , Html.div
                [ localClass "loginForm_notes_text"
                ]
                [ Html.text "Password: guest"
                ]
            ]
        , if memory.showError && List.length errors > 0 then
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
        ]



-- Procedures


{-| -}
type Command
    = ToastCommand Toast.Command
    | RequestLogin Login.Login (Result Http.Error Value -> Msg Event)


{-| -}
runCommand : Command -> Cmd (Msg Event)
runCommand cmd =
    case cmd of
        ToastCommand toastCommand ->
            Toast.runCommand toastCommand
                |> Cmd.map (Tepa.mapMsg ToastEvent)

        RequestLogin login toMsg ->
            Login.request login toMsg


type alias Promise a =
    Tepa.Promise Command Memory Event a


type alias Pointer m =
    Tepa.Pointer Memory m


type alias Bucket =
    { key : NavKey
    , requestUrl : Url
    , toastPointer : Pointer Toast.Memory
    }


{-| -}
currentSession : Promise (Maybe Session)
currentSession =
    Tepa.currentState
        |> Tepa.map (\m -> m.msession)



-- -- Initialization


{-| -}
procedure : Url -> NavKey -> Promise Void
procedure url key =
    -- Initialize Widget
    Tepa.putMaybeLayer
        { get = .toast
        , set = \toast m -> { m | toast = toast }
        , init = Toast.init
        }
        |> Tepa.andThen
            (\toastPointer ->
                let
                    bucket =
                        { key = key
                        , requestUrl = url
                        , toastPointer = toastPointer
                        }
                in
                -- Main Procedures
                Tepa.syncAll
                    [ loginFormProcedure bucket
                    ]
            )


loginFormProcedure : Bucket -> Promise Void
loginFormProcedure bucket =
    let
        modifyForm f =
            Tepa.modify <|
                \m ->
                    { m
                        | loginForm =
                            let
                                loginForm =
                                    m.loginForm
                            in
                            { loginForm
                                | form = f loginForm.form
                            }
                    }
    in
    Tepa.withLayerEvent <|
        \e ->
            case e of
                ChangeLoginId str ->
                    [ modifyForm <|
                        \m -> { m | id = str }
                    , Tepa.lazy <|
                        \_ -> loginFormProcedure bucket
                    ]

                ChangeLoginPass str ->
                    [ modifyForm <|
                        \m -> { m | pass = str }
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
        , Tepa.currentState
            |> Tepa.andThen
                (\curr ->
                    case Login.fromForm curr.loginForm.form of
                        Err _ ->
                            Tepa.sequence
                                [ modifyLoginForm <|
                                    \m ->
                                        { m
                                            | isBusy = False
                                            , showError = True
                                        }
                                , Tepa.lazy <| \_ -> loginFormProcedure bucket
                                ]

                        Ok login ->
                            requestLogin login
                                |> Tepa.andThen
                                    (\response ->
                                        case response of
                                            Err err ->
                                                Tepa.syncAll
                                                    [ Toast.pushHttpError err
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

                                            Ok resp ->
                                                Tepa.sequence
                                                    [ Tepa.modify <|
                                                        \m ->
                                                            { m
                                                                | msession = Just resp.session
                                                                , loginForm =
                                                                    let
                                                                        loginForm =
                                                                            m.loginForm
                                                                    in
                                                                    { loginForm
                                                                        | isBusy = False
                                                                    }
                                                            }
                                                    , Nav.pushRoute bucket.key (Nav.extractRoute bucket.requestUrl)
                                                    ]
                                    )
                )
        ]


requestLogin : Login.Login -> Promise (Result Http.Error Login.Response)
requestLogin login =
    Tepa.httpRequest
        { name = "requestLogin"
        , request = RequestLogin login
        , decoder = Login.responseDecoder
        }



-- Toast


runToastPromise :
    Pointer Toast.Memory
    -> Tepa.Promise Toast.Command Toast.Memory Toast.Event a
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
        |> Tepa.mapCmd ToastCommand



-- Scenario


{-| -}
type alias ScenarioSet flags c m e =
    { changeLoginId : String -> Scenario flags c m e
    , changeLoginPass : String -> Scenario flags c m e
    , clickSubmitLogin : Scenario flags c m e
    , receiveLoginResp : Result Http.Error Value -> Scenario flags c m e
    , expectAvailable : String -> Scenario flags c m e
    , expectLoginFormShowNoErrors : String -> Scenario flags c m e
    , expectLoginFormShowError : String -> Scenario flags c m e
    , toast : Toast.ScenarioSet flags c m e
    }


type alias ScenarioProps c m e =
    { querySelf : LayerQuery m Memory
    , wrapEvent : Event -> e
    , unwrapCommand : c -> Maybe Command
    , session : Scenario.Session
    }


{-| -}
scenario : ScenarioProps c m e -> ScenarioSet flags c m e
scenario props =
    { changeLoginId = changeLoginId props
    , changeLoginPass = changeLoginPass props
    , clickSubmitLogin = clickSubmitLogin props
    , receiveLoginResp = receiveLoginResp props
    , expectAvailable = expectAvailable props
    , expectLoginFormShowNoErrors = expectLoginFormShowNoErrors props
    , expectLoginFormShowError = expectLoginFormShowError props
    , toast =
        Toast.scenario
            { querySelf =
                props.querySelf
                    |> LayerQuery.child .toast
            , wrapEvent = ToastEvent >> props.wrapEvent
            , unwrapCommand =
                \c ->
                    case props.unwrapCommand c of
                        Just (ToastCommand c1) ->
                            Just c1

                        _ ->
                            Nothing
            , session = props.session
            }
    }


changeLoginId : ScenarioProps c m e -> String -> Scenario flags c m e
changeLoginId props str =
    Scenario.userEvent props.session
        ("Type \"" ++ str ++ "\" for Login ID field")
        { target = props.querySelf
        , event =
            ChangeLoginId str
                |> props.wrapEvent
        }


changeLoginPass : ScenarioProps c m e -> String -> Scenario flags c m e
changeLoginPass props str =
    Scenario.userEvent props.session
        ("Type \"" ++ str ++ "\" for Login Password field")
        { target = props.querySelf
        , event =
            ChangeLoginPass str
                |> props.wrapEvent
        }


clickSubmitLogin : ScenarioProps c m e -> Scenario flags c m e
clickSubmitLogin props =
    Scenario.userEvent props.session
        "Click \"Login\" button."
        { target = props.querySelf
        , event =
            ClickSubmitLogin
                |> props.wrapEvent
        }


receiveLoginResp : ScenarioProps c m e -> Result Http.Error Value -> Scenario flags c m e
receiveLoginResp props res =
    Scenario.customResponse props.session
        "Backend responds to the login request."
        { target = props.querySelf
        , response =
            \cmd ->
                case props.unwrapCommand cmd of
                    Just (RequestLogin _ toMsg) ->
                        toMsg res
                            |> Tepa.mapMsg props.wrapEvent
                            |> Just

                    _ ->
                        Nothing
        }


expectLoginFormShowNoErrors : ScenarioProps c m e -> String -> Scenario flags c m e
expectLoginFormShowNoErrors props description =
    Scenario.expectAppView props.session
        description
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


expectAvailable : ScenarioProps c m e -> String -> Scenario flags c m e
expectAvailable props str =
    Scenario.expectMemory props.session
        str
        { target = props.querySelf
        , expectation = Expect.Builder.pass
        }


expectLoginFormShowError : ScenarioProps c m e -> String -> Scenario flags c m e
expectLoginFormShowError props str =
    Scenario.expectAppView props.session
        ("The login form shows an error: " ++ str)
        { expectation =
            \{ body } ->
                Query.fromHtml (Html.div [] body)
                    |> Query.find
                        [ localClassSelector "loginForm"
                        ]
                    |> Query.findAll
                        [ localClassSelector "loginForm_errorField_error"
                        , Selector.text str
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
