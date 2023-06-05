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

import App.Path as Path
import App.Session exposing (Session)
import AppUrl exposing (AppUrl)
import Dict
import Expect
import Json.Encode exposing (Value)
import Mixin exposing (Mixin)
import Mixin.Events as Events
import Mixin.Html as Html exposing (Html)
import Page.Home.EditAccount as EditAccount
import Tepa exposing (Layer, NavKey, Void)
import Tepa.Http as Http
import Tepa.Navigation as Nav
import Tepa.Scenario as Scenario exposing (Scenario)
import Tepa.Time as Time exposing (Posix)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Widget.Toast as Toast


{-| -}
type alias Memory =
    { session : Session
    , toast : Toast.Memory
    , clock : ClockMemory
    , editAccountForm : EditAccountFormMemory
    }


{-| -}
type alias ClockMemory =
    { currentTime : Posix
    , zone : Time.Zone
    }


{-| -}
init : Session -> Tepa.Promise m e Memory
init session =
    Tepa.succeed
        (\toast now zone ->
            { session = session
            , toast = toast
            , clock =
                { currentTime = now
                , zone = zone
                }
            , editAccountForm = initEditAccountForm session
            }
        )
        |> Tepa.sync Toast.init
        |> Tepa.sync Time.now
        |> Tepa.sync Time.here


{-| -}
type Event
    = ToastEvent Toast.Event
    | ChangeEditAccountFormAccountId String
    | ClickSubmitEditAccount



-- View


type alias Msg =
    Tepa.Msg Event


{-| -}
view : Layer Memory -> Html Msg
view =
    Tepa.layerView <|
        \state ->
            Html.div
                [ localClass "page"
                ]
                [ clockView state.clock
                , Html.div
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
                            (AppUrl.toString
                                { path =
                                    [ Path.prefix
                                    , "users"
                                    ]
                                , queryParameters = Dict.empty
                                , fragment = Nothing
                                }
                            )
                        ]
                        [ Html.text "Users"
                        ]
                    ]
                , Toast.view state.toast
                    |> Html.map (Tepa.mapMsg ToastEvent)
                ]


clockView : ClockMemory -> Html Msg
clockView state =
    Html.div
        [ localClass "clock"
        ]
        [ Html.text <| formatTime state.zone state.currentTime
        ]


formatTime : Time.Zone -> Posix -> String
formatTime zone time =
    String.concat
        [ Time.toYear zone time
            |> String.fromInt
        , "-"
        , case Time.toMonth zone time of
            Time.Jan ->
                "01"

            Time.Feb ->
                "02"

            Time.Mar ->
                "03"

            Time.Apr ->
                "04"

            Time.May ->
                "05"

            Time.Jun ->
                "06"

            Time.Jul ->
                "07"

            Time.Aug ->
                "08"

            Time.Sep ->
                "09"

            Time.Oct ->
                "10"

            Time.Nov ->
                "11"

            Time.Dec ->
                "12"
        , "-"
        , Time.toDay zone time
            |> String.fromInt
            |> String.padLeft 2 '0'
        , " "
        , Time.toHour zone time
            |> String.fromInt
            |> String.padLeft 2 '0'
        , ":"
        , Time.toMinute zone time
            |> String.fromInt
            |> String.padLeft 2 '0'
        , ":"
        , Time.toSecond zone time
            |> String.fromInt
            |> String.padLeft 2 '0'
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


editAccountFormView : EditAccountFormMemory -> Html Msg
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


type alias Bucket =
    { key : NavKey
    , requestPath : AppUrl
    }



-- -- Initialization


{-| -}
procedure : NavKey -> AppUrl -> Promise Void
procedure key url =
    let
        bucket =
            { key = key
            , requestPath = url
            }
    in
    -- Main Procedures
    Tepa.syncAll
        [ clockProcedure
        , editAccountFormProcedure bucket
        ]


clockProcedure : Promise Void
clockProcedure =
    let
        modifyClock f =
            Tepa.modify <|
                \m ->
                    { m | clock = f m.clock }
    in
    Time.every 500 <|
        \time ->
            [ modifyClock <|
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
                                                |> runToastPromise
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
                                                |> runToastPromise
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
    Tepa.Promise Toast.Memory Toast.Event a
    -> Promise a
runToastPromise prom =
    Tepa.liftMemory
        { get = .toast
        , set = \toast m -> { m | toast = toast }
        }
        prom
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
    , expectGreetingMessage :
        { value : String
        }
        -> Scenario.Markup
        -> Scenario flags m e
    , expectClockMessage :
        { value : String
        }
        -> Scenario.Markup
        -> Scenario flags m e
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
    , expectGreetingMessage = expectGreetingMessage props
    , expectClockMessage = expectClockMessage props
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


expectGreetingMessage :
    ScenarioProps m e
    ->
        { value : String
        }
    -> Scenario.Markup
    -> Scenario flags m e
expectGreetingMessage props { value } markup =
    Scenario.expectAppView props.session
        markup
        { expectation =
            \{ body } ->
                Query.fromHtml (Html.div [] body)
                    |> Query.find
                        [ localClassSelector "greeting"
                        ]
                    |> Query.findAll
                        [ localClassSelector "greeting_name"
                        , Selector.exactText value
                        ]
                    |> Query.count (Expect.equal 1)
        }


expectClockMessage :
    ScenarioProps m e
    ->
        { value : String
        }
    -> Scenario.Markup
    -> Scenario flags m e
expectClockMessage props { value } markup =
    Scenario.expectAppView props.session
        markup
        { expectation =
            \{ body } ->
                Query.fromHtml (Html.div [] body)
                    |> Query.find
                        [ localClassSelector "clock"
                        ]
                    |> Query.has
                        [ Selector.exactText value
                        ]
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
