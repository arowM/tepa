module Page.Home exposing
    ( Memory
    , ClockMemory
    , ScenarioProps
    , ScenarioSet
    , init
    , procedure
    , scenario
    , view
    )

{-| Home page.

@docs Memory
@docs ClockMemory
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
import Mixin.Html as Html exposing (Html)
import Page.Home.EditAccount as EditAccount
import Tepa exposing (Layer, Msg, NavKey, Promise, ViewContext, Void)
import Tepa.Http as Http
import Tepa.Navigation as Nav
import Tepa.Scenario as Scenario exposing (Scenario)
import Tepa.Time as Time exposing (Posix)
import Test.Html.Event as HtmlEvent
import Test.Html.Event.Extra as HtmlEvent
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
init : Session -> Promise m Memory
init session =
    Tepa.succeed
        (\toast now zone ->
            { session = session
            , toast = toast
            , clock =
                { currentTime = now
                , zone = zone
                }
            , editAccountForm =
                { isBusy = False

                -- Do not show errors initially to avoid bothering
                -- the user with "Input required" errors
                -- when they has not yet entered the information.
                , showError = False
                }
            }
        )
        |> Tepa.sync Toast.init
        |> Tepa.sync Time.now
        |> Tepa.sync Time.here



-- View


{-| -}
view : Layer Memory -> Html Msg
view =
    Tepa.layerView <|
        \context ->
            Html.div
                [ localClass "page"
                ]
                [ clockView (Tepa.mapViewContext .clock context)
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
                        [ context.state.session.profile.name
                            |> Maybe.withDefault "(Unknown)"
                            |> Html.text
                        ]
                    , Html.span
                        [ localClass "greeting_text"
                        ]
                        [ Html.text "!"
                        ]
                    ]
                , editAccountFormView (Tepa.mapViewContext .editAccountForm context)
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
                , Toast.view
                    (Tepa.mapViewContext .toast context)
                ]


clockView : ViewContext ClockMemory -> Html Msg
clockView { state } =
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
    { isBusy : Bool
    , showError : Bool
    }


editAccountFormView : ViewContext EditAccountFormMemory -> Html Msg
editAccountFormView { state, setKey, values } =
    let
        errors =
            EditAccount.toFormErrors values
    in
    Html.div
        [ localClass "editAccountForm"
        , Mixin.boolAttribute "aria-invalid"
            (state.showError && not (List.isEmpty errors))
        ]
        [ Html.node "label"
            [ localClass "editAccountForm_id_label"
            ]
            [ Html.text "New name:"
            , Html.node "input"
                [ Mixin.attribute "type" "text"
                , Mixin.disabled state.isBusy
                , localClass "editAccountForm_id_input"
                , setKey EditAccount.keys.editAccountFormName
                    |> Mixin.fromAttributes
                ]
                []
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
        , Html.node "button"
            [ localClass "editAccountForm_saveButton"
            , Mixin.disabled state.isBusy
            , setKey keys.editAccountFormSaveButton
                |> Mixin.fromAttributes
            ]
            [ Html.text "Save"
            ]
        ]


keys :
    { editAccountFormSaveButton : String
    }
keys =
    { editAccountFormSaveButton = "editAccountFormSaveButton"
    }



-- Procedures


type alias Bucket =
    { key : NavKey
    , requestPath : AppUrl
    }



-- -- Initialization


{-| -}
procedure : NavKey -> AppUrl -> Promise Memory Void
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
        , Tepa.bind Tepa.currentState <|
            \{ session } ->
                [ Tepa.setValue EditAccount.keys.editAccountFormName <|
                    Maybe.withDefault "" session.profile.name
                , editAccountFormProcedure bucket
                ]
        ]


clockProcedure : Promise Memory Void
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


editAccountFormProcedure : Bucket -> Promise Memory Void
editAccountFormProcedure bucket =
    Tepa.sequence
        [ Tepa.awaitViewEvent
            { key = keys.editAccountFormSaveButton
            , type_ = "click"
            }
        , Tepa.lazy <| \_ -> submitAccountProcedure bucket
        ]


submitAccountProcedure : Bucket -> Promise Memory Void
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
        , Tepa.bind Tepa.getValues <|
            \form ->
                case EditAccount.fromForm form of
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
    , changeEditAccountFormAccountId :
        { value : String
        }
        -> Scenario.Markup
        -> Scenario flags m
    , clickSubmitEditAccount :
        Scenario.Markup -> Scenario flags m
    , receiveEditAccountResp :
        (Value -> Maybe ( Http.Metadata, String ))
        -> Scenario.Markup
        -> Scenario flags m
    , expectAvailable :
        Scenario.Markup -> Scenario flags m
    , expectEditAccountFormShowNoErrors :
        Scenario.Markup -> Scenario flags m
    , expectGreetingMessage :
        { value : String
        }
        -> Scenario.Markup
        -> Scenario flags m
    , expectClockMessage :
        { value : String
        }
        -> Scenario.Markup
        -> Scenario flags m
    , editAccountEndpoint :
        { method : String
        , url : String
        }
    }


{-| -}
type alias ScenarioProps m =
    { querySelf : Layer m -> Maybe (Layer Memory)
    , session : Scenario.Session
    }


{-| -}
scenario : ScenarioProps m -> ScenarioSet flags m
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


changeEditAccountFormAccountId : ScenarioProps m -> { value : String } -> Scenario.Markup -> Scenario flags m
changeEditAccountFormAccountId props { value } markup =
    Scenario.userOperation props.session
        markup
        { query =
            Query.find
                [ localClassSelector "editAccountForm_id_input"
                ]
        , operation = HtmlEvent.change value
        }


clickSubmitEditAccount : ScenarioProps m -> Scenario.Markup -> Scenario flags m
clickSubmitEditAccount props markup =
    Scenario.userOperation props.session
        markup
        { query =
            Query.find
                [ localClassSelector "editAccountForm_saveButton"
                , Selector.disabled False
                ]
        , operation =
            HtmlEvent.click
        }


receiveEditAccountResp : ScenarioProps m -> (Value -> Maybe ( Http.Metadata, String )) -> Scenario.Markup -> Scenario flags m
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


expectAvailable : ScenarioProps m -> Scenario.Markup -> Scenario flags m
expectAvailable props markup =
    Scenario.expectMemory props.session
        markup
        { layer = props.querySelf
        , expectation = \_ -> Expect.pass
        }


expectEditAccountFormShowNoErrors : ScenarioProps m -> Scenario.Markup -> Scenario flags m
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
    ScenarioProps m
    ->
        { value : String
        }
    -> Scenario.Markup
    -> Scenario flags m
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
    ScenarioProps m
    ->
        { value : String
        }
    -> Scenario.Markup
    -> Scenario flags m
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
