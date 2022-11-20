module Page.Home exposing
    ( Command
    , Event
    , Memory
    , Promise
    , ScenarioProps
    , ScenarioSet
    , currentSession
    , init
    , procedure
    , runCommand
    , scenario
    , view
    )

import App.Route as Route
import App.Session exposing (Session)
import Expect
import Expect.Builder
import Http
import Json.Encode exposing (Value)
import Mixin exposing (Mixin)
import Mixin.Events as Events
import Mixin.Html as Html exposing (Html)
import Page.Home.EditAccount as EditAccount
import Tepa exposing (Layer, Msg, Void)
import Tepa.Navigation exposing (NavKey)
import Tepa.Scenario as Scenario exposing (Scenario)
import Tepa.Scenario.LayerQuery exposing (LayerQuery)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Widget.Toast as Toast


{-| -}
type alias Memory =
    { session : Session
    , toast : Maybe (Layer Toast.Memory)
    , editAccountForm : EditAccountFormMemory
    }


{-| -}
init : Session -> Memory
init session =
    { session = session
    , toast = Nothing
    , editAccountForm = initEditAccountForm session
    }


{-| -}
type Event
    = ToastEvent Toast.Event
    | ChangeEditAccountFormAccountId String
    | ClickSubmitEditAccount



-- View


{-| -}
view : Layer Memory -> Html (Msg Event)
view =
    Tepa.layerView <|
        \memory ->
            Html.div
                [ localClass "page"
                ]
                [ editAccountFormView memory.editAccountForm
                , Html.div
                    [ localClass "dashboard_links"
                    ]
                    [ Html.a
                        [ localClass "dashboard_links_linkButton-users"
                        , Mixin.attribute "href" <| Route.toPath Route.Users
                        ]
                        [ Html.text "Users"
                        ]
                    ]
                , case memory.toast of
                    Nothing ->
                        Html.text ""

                    Just toast ->
                        Toast.view toast
                            |> Html.map (Tepa.mapMsg ToastEvent)
                ]



-- -- EditAccountForm


type alias EditAccountFormMemory =
    { form : EditAccount.Form
    , isBusy : Bool
    , showError : Bool
    }


initEditAccountForm : Session -> EditAccountFormMemory
initEditAccountForm session =
    { form = EditAccount.initForm session.id
    , isBusy = False

    -- Do not show errors initially to avoid bothering
    -- the user with "Input required" errors
    -- when they has not yet entered the information.
    , showError = False
    }


editAccountFormView : EditAccountFormMemory -> Html (Msg Event)
editAccountFormView memory =
    let
        errors =
            EditAccount.toFormErrors memory.form
    in
    Html.div
        [ localClass "editAccountForm"
        , Mixin.boolAttribute "aria-invalid"
            (memory.showError && not (List.isEmpty errors))
        ]
        [ Html.node "label"
            [ localClass "editAccountForm_label-id"
            ]
            [ Html.text "New Account ID:"
            , Html.node "input"
                [ Mixin.attribute "type" "text"
                , Mixin.attribute "value" memory.form.id
                , Mixin.boolAttribute "disabled" memory.isBusy
                , Events.onChange ChangeEditAccountFormAccountId
                    |> Tepa.eventMixin
                ]
                []
            ]
        , Html.div
            [ localClass "editAccountForm_buttonGroup"
            ]
            [ Html.node "button"
                [ localClass "editAccountForm_buttonGroup_button-submit"
                , Mixin.boolAttribute "disabled" memory.isBusy
                , Events.onClick ClickSubmitEditAccount
                    |> Tepa.eventMixin
                ]
                [ Html.text "Save"
                ]
            ]
        , if memory.showError && List.length errors > 0 then
            Html.div
                [ localClass "editAccountForm_errorField"
                ]
                (List.map
                    (\err ->
                        Html.div
                            [ localClass "editAccountForm_errorField_error"
                            ]
                            [ Html.text <| EditAccount.displayFormError err
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
    | RequestEditAccount EditAccount.EditAccount (Result Http.Error Value -> Msg Event)


{-| -}
runCommand : Command -> Cmd (Msg Event)
runCommand cmd =
    case cmd of
        ToastCommand toastCommand ->
            Toast.runCommand toastCommand
                |> Cmd.map (Tepa.mapMsg ToastEvent)

        RequestEditAccount editAccount toMsg ->
            EditAccount.request editAccount toMsg


{-| -}
type alias Promise a =
    Tepa.Promise Command Memory Event a


type alias Pointer m =
    Tepa.Pointer Memory m


type alias Bucket =
    { key : NavKey
    , toastPointer : Pointer Toast.Memory
    }


{-| -}
currentSession : Promise Session
currentSession =
    Tepa.currentState
        |> Tepa.map (\m -> m.session)



-- -- Initialization


{-| -}
procedure : NavKey -> Promise Void
procedure key =
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
                        , toastPointer = toastPointer
                        }
                in
                -- Main Procedures
                Tepa.syncAll
                    [ editAccountFormProcedure bucket
                    ]
            )


editAccountFormProcedure : Bucket -> Promise Void
editAccountFormProcedure bucket =
    let
        modifyForm f =
            Tepa.modify <|
                \m ->
                    { m
                        | editAccountForm =
                            let
                                editAccountForm =
                                    m.editAccountForm
                            in
                            { editAccountForm
                                | form = f editAccountForm.form
                            }
                    }
    in
    Tepa.withLayerEvent <|
        \e ->
            case e of
                ChangeEditAccountFormAccountId str ->
                    [ modifyForm <|
                        \m -> { m | id = str }
                    , Tepa.lazy <|
                        \_ -> editAccountFormProcedure bucket
                    ]

                ClickSubmitEditAccount ->
                    [ Tepa.lazy <| \_ -> submitAccountProcedure bucket
                    ]

                _ ->
                    []


submitAccountProcedure : Bucket -> Promise Void
submitAccountProcedure bucket =
    let
        modifyEditAccountForm f =
            Tepa.modify <|
                \m ->
                    { m
                        | editAccountForm = f m.editAccountForm
                    }
    in
    Tepa.sequence
        [ modifyEditAccountForm <|
            \m -> { m | isBusy = True }
        , Tepa.currentState
            |> Tepa.andThen
                (\curr ->
                    case EditAccount.fromForm curr.editAccountForm.form of
                        Err _ ->
                            Tepa.sequence
                                [ modifyEditAccountForm <|
                                    \m ->
                                        { m
                                            | isBusy = False
                                            , showError = True
                                        }
                                , Tepa.lazy <| \_ -> editAccountFormProcedure bucket
                                ]

                        Ok editAccount ->
                            requestEditAccount editAccount
                                |> Tepa.andThen
                                    (\response ->
                                        case response of
                                            Err err ->
                                                Tepa.syncAll
                                                    [ Toast.pushHttpError err
                                                        |> runToastPromise bucket.toastPointer
                                                        |> Tepa.void
                                                    , Tepa.sequence
                                                        [ modifyEditAccountForm <|
                                                            \m ->
                                                                { m | isBusy = False }
                                                        , Tepa.lazy <|
                                                            \_ ->
                                                                editAccountFormProcedure bucket
                                                        ]
                                                    ]

                                            Ok resp ->
                                                Tepa.sequence
                                                    [ Tepa.modify <|
                                                        \m ->
                                                            { m
                                                                | session = resp.session
                                                                , editAccountForm =
                                                                    let
                                                                        editAccountForm =
                                                                            m.editAccountForm
                                                                    in
                                                                    { editAccountForm
                                                                        | isBusy = False
                                                                    }
                                                            }
                                                    , Tepa.lazy <|
                                                        \_ -> editAccountFormProcedure bucket
                                                    ]
                                    )
                )
        ]


requestEditAccount : EditAccount.EditAccount -> Promise (Result Http.Error EditAccount.Response)
requestEditAccount editAccount =
    Tepa.httpRequest
        { name = "requestEditAccount"
        , request = RequestEditAccount editAccount
        , decoder = EditAccount.responseDecoder
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
    { changeEditAccountFormAccountId : String -> Scenario flags c m e
    , clickSubmitEditAccount : Scenario flags c m e
    , receiveEditAccountResp : Result Http.Error Value -> Scenario flags c m e
    , expectAvailable : String -> Scenario flags c m e
    , expectEditAccountFormShowNoErrors : Scenario flags c m e
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
    { changeEditAccountFormAccountId = changeEditAccountFormAccountId props
    , clickSubmitEditAccount = clickSubmitEditAccount props
    , receiveEditAccountResp = receiveEditAccountResp props
    , expectAvailable = expectAvailable props
    , expectEditAccountFormShowNoErrors = expectEditAccountFormShowNoErrors props
    }


changeEditAccountFormAccountId : ScenarioProps c m e -> String -> Scenario flags c m e
changeEditAccountFormAccountId props str =
    Scenario.layerEvent props.session
        ("Type \"" ++ str ++ "\" for Account ID field")
        { target = props.querySelf
        , event =
            ChangeEditAccountFormAccountId str
                |> props.wrapEvent
        }


clickSubmitEditAccount : ScenarioProps c m e -> Scenario flags c m e
clickSubmitEditAccount props =
    Scenario.layerEvent props.session
        "Click \"Save\" button for edit account form."
        { target = props.querySelf
        , event =
            ClickSubmitEditAccount
                |> props.wrapEvent
        }


receiveEditAccountResp : ScenarioProps c m e -> Result Http.Error Value -> Scenario flags c m e
receiveEditAccountResp props res =
    Scenario.customResponse props.session
        "Backend responds to the edit account request."
        { target = props.querySelf
        , response =
            \cmd ->
                case props.unwrapCommand cmd of
                    Just (RequestEditAccount _ toMsg) ->
                        toMsg res
                            |> Tepa.mapMsg props.wrapEvent
                            |> Just

                    _ ->
                        Nothing
        }


expectAvailable : ScenarioProps c m e -> String -> Scenario flags c m e
expectAvailable props str =
    Scenario.expectMemory props.session
        str
        { target = props.querySelf
        , expectation = Expect.Builder.pass
        }


expectEditAccountFormShowNoErrors : ScenarioProps c m e -> Scenario flags c m e
expectEditAccountFormShowNoErrors props =
    Scenario.expectAppView props.session
        "The edit account form shows no errors at this point."
        { expectation =
            \{ body } ->
                Query.fromHtml (Html.div [] body)
                    |> Query.find
                        [ localClassSelector "editAccountForm"
                        ]
                    |> Query.findAll
                        [ localClassSelector "editAccountForm_errorField_error"
                        ]
                    |> Query.count (Expect.equal 0)
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
    "page_home--"
