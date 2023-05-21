module Page.Home exposing
    ( Event
    , Memory
    , ClockMemory
    , Promise
    , ScenarioProps
    , ScenarioSet
    , init
    , procedure
    , scenario
    , view
    )

{-| Home page.

@docs Event
@docs Memory
@docs ClockMemory
@docs Promise
@docs ScenarioProps
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
import Page.Home.EditAccount as EditAccount
import Tepa exposing (Layer, Msg, NavKey, Void)
import Tepa.AbsolutePath as AbsolutePath exposing (AbsolutePath)
import Tepa.Http as Http
import Tepa.Navigation as Nav
import Tepa.Scenario as Scenario exposing (Scenario)
import Tepa.Time as Time exposing (Posix)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Url exposing (Url)
import Widget.Toast as Toast


{-| -}
type alias Memory =
    { session : Session
    , toast : Maybe (Layer Toast.Memory)
    , editAccountForm : EditAccountFormMemory
    , clock : Maybe (Layer ClockMemory)
    }


{-| -}
init : Session -> Memory
init session =
    { session = session
    , toast = Nothing
    , editAccountForm = initEditAccountForm session
    , clock = Nothing
    }


{-| -}
type alias ClockMemory =
    { currentTime : Posix
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
        \state ->
            Html.div
                [ localClass "page"
                ]
                [ Html.div
                    [ localClass "greeting"
                    ]
                    [ Html.span
                        [ localClass "greeting_text"
                        ]
                        [ Html.text "Hi, "
                        ]
                    , Html.span
                        [ localClass "greeting_name"
                        ]
                        [ state.session.profile.name
                            |> Maybe.withDefault "(Unknown)"
                            |> Html.text
                        ]
                    , Html.span
                        [ localClass "greeting_text"
                        ]
                        [ Html.text "!"
                        ]
                    ]
                , editAccountFormView state.editAccountForm
                , Html.div
                    [ localClass "dashboard_links"
                    ]
                    [ Html.a
                        [ localClass "dashboard_links_linkButton-users"
                        , Mixin.attribute "href"
                            (Route.toAbsolutePath Route.Users
                                |> AbsolutePath.toString
                            )
                        ]
                        [ Html.text "Users"
                        ]
                    ]
                , case state.toast of
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
    { form =
        { name = Maybe.withDefault "" session.profile.name
        }
    , isBusy = False

    -- Do not show errors initially to avoid bothering
    -- the user with "Input required" errors
    -- when they has not yet entered the information.
    , showError = False
    }


editAccountFormView : EditAccountFormMemory -> Html (Msg Event)
editAccountFormView state =
    let
        errors =
            EditAccount.toFormErrors state.form
    in
    Html.div
        [ localClass "editAccountForm"
        , Mixin.boolAttribute "aria-invalid"
            (state.showError && not (List.isEmpty errors))
        ]
        [ Html.node "label"
            [ localClass "editAccountForm_label-id"
            ]
            [ Html.text "New name:"
            , Html.node "input"
                [ Mixin.attribute "type" "text"
                , Mixin.attribute "value" state.form.name
                , Mixin.disabled state.isBusy
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
                , Mixin.disabled state.isBusy
                , Events.onClick ClickSubmitEditAccount
                    |> Tepa.eventMixin
                ]
                [ Html.text "Save"
                ]
            ]
        , if state.showError && List.length errors > 0 then
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
type alias Promise a =
    Tepa.Promise Memory Event a


type alias Pointer m =
    Tepa.Pointer Memory m


type alias Bucket =
    { key : NavKey
    , requestPath : AbsolutePath
    , toastPointer : Pointer Toast.Memory
    , clockPointer : Pointer ClockMemory
    }



-- -- Initialization


{-| -}
procedure : NavKey -> Url -> Promise Void
procedure key url =
    Tepa.bind2
        -- Initialize Toast Widget
        (Tepa.maybeLayer
            { get = .toast
            , set = \toast m -> { m | toast = toast }
            , init = Toast.init
            }
        )
        -- Initialize Clock
        (Tepa.bindAndThen Time.now <|
            \now ->
                Tepa.maybeLayer
                    { get = .clock
                    , set = \clock m -> { m | clock = clock }
                    , init =
                        { currentTime = now
                        }
                    }
        )
    <|
        \toastPointer clockPointer ->
            let
                bucket =
                    { key = key
                    , requestPath = AbsolutePath.fromUrl url
                    , toastPointer = toastPointer
                    , clockPointer = clockPointer
                    }
            in
            -- Main Procedures
            [ Tepa.syncAll
                [ clockProcedure bucket
                , editAccountFormProcedure bucket
                ]
            ]


clockProcedure : Bucket -> Promise Void
clockProcedure bucket =
    Time.every 500 <|
        \time ->
            [ Tepa.onLayer bucket.clockPointer <|
                Tepa.modify <|
                    \m -> { m | currentTime = time }
            ]


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
                        \m -> { m | name = str }
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
        , Tepa.bind Tepa.currentState <|
            \curr ->
                case EditAccount.fromForm curr.editAccountForm.form of
                    Err _ ->
                        [ modifyEditAccountForm <|
                            \m ->
                                { m
                                    | isBusy = False
                                    , showError = True
                                }
                        , Tepa.lazy <| \_ -> editAccountFormProcedure bucket
                        ]

                    Ok editAccount ->
                        [ Tepa.bind
                            (EditAccount.request editAccount)
                          <|
                            \response ->
                                case response of
                                    EditAccount.TemporaryErrorResponse ->
                                        [ Tepa.syncAll
                                            [ Toast.pushError
                                                "Network error, please check your network and try again."
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
                                        ]

                                    EditAccount.FatalErrorResponse ->
                                        [ Tepa.syncAll
                                            [ Toast.pushError
                                                "Internal error, please contact our support team."
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
                                        ]

                                    EditAccount.LoginRequiredResponse ->
                                        [ Nav.pushPath bucket.key bucket.requestPath
                                        ]

                                    EditAccount.GoodResponse resp ->
                                        [ Tepa.modify <|
                                            \m ->
                                                { m
                                                    | session =
                                                        let
                                                            session =
                                                                m.session
                                                        in
                                                        { session
                                                            | profile =
                                                                let
                                                                    profile =
                                                                        session.profile
                                                                in
                                                                { profile | name = Just resp.name }
                                                        }
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
    , changeEditAccountFormAccountId :
        { value : String
        }
        -> Scenario.Markup
        -> Scenario flags m e
    , clickSubmitEditAccount :
        Scenario.Markup -> Scenario flags m e
    , receiveEditAccountResp :
        (Value -> Maybe ( Http.Metadata, String ))
        -> Scenario.Markup
        -> Scenario flags m e
    , expectAvailable :
        Scenario.Markup -> Scenario flags m e
    , expectEditAccountFormShowNoErrors :
        Scenario.Markup -> Scenario flags m e
    , editAccountEndpoint :
        { method : String
        , url : String
        }
    }


{-| -}
type alias ScenarioProps m e =
    { querySelf : Layer m -> Maybe (Layer Memory)
    , wrapEvent : Event -> e
    , session : Scenario.Session
    }


{-| -}
scenario : ScenarioProps m e -> ScenarioSet flags m e
scenario props =
    { layer = props.querySelf
    , changeEditAccountFormAccountId = changeEditAccountFormAccountId props
    , clickSubmitEditAccount = clickSubmitEditAccount props
    , receiveEditAccountResp = receiveEditAccountResp props
    , expectAvailable = expectAvailable props
    , expectEditAccountFormShowNoErrors = expectEditAccountFormShowNoErrors props
    , editAccountEndpoint =
        { method = EditAccount.method
        , url = EditAccount.endpointUrl
        }
    }


changeEditAccountFormAccountId : ScenarioProps m e -> { value : String } -> Scenario.Markup -> Scenario flags m e
changeEditAccountFormAccountId props { value } markup =
    Scenario.layerEvent props.session
        markup
        { layer = props.querySelf
        , event =
            ChangeEditAccountFormAccountId value
                |> props.wrapEvent
        }


clickSubmitEditAccount : ScenarioProps m e -> Scenario.Markup -> Scenario flags m e
clickSubmitEditAccount props markup =
    Scenario.layerEvent props.session
        markup
        { layer = props.querySelf
        , event =
            ClickSubmitEditAccount
                |> props.wrapEvent
        }


receiveEditAccountResp : ScenarioProps m e -> (Value -> Maybe ( Http.Metadata, String )) -> Scenario.Markup -> Scenario flags m e
receiveEditAccountResp props toResponse markup =
    Scenario.httpResponse props.session
        markup
        { layer = props.querySelf
        , response =
            \rawRequest ->
                if rawRequest.url == EditAccount.endpointUrl then
                    case rawRequest.requestBody of
                        Scenario.JsonHttpRequestBody value ->
                            toResponse value

                        _ ->
                            Nothing

                else
                    Nothing
        }


expectAvailable : ScenarioProps m e -> Scenario.Markup -> Scenario flags m e
expectAvailable props markup =
    Scenario.expectMemory props.session
        markup
        { layer = props.querySelf
        , expectation = \_ -> Expect.pass
        }


expectEditAccountFormShowNoErrors : ScenarioProps m e -> Scenario.Markup -> Scenario flags m e
expectEditAccountFormShowNoErrors props markup =
    Scenario.expectAppView props.session
        markup
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
