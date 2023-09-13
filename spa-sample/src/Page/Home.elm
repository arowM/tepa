module Page.Home exposing
    ( Memory
    , ClockMemory
    , ScenarioProps
    , ScenarioSet
    , init
    , leave
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
import Page.Home.EditAccount as EditAccount
import Tepa exposing (Layer, NavKey, Promise, ViewContext)
import Tepa.Html as Html exposing (Html)
import Tepa.HtmlSelector as Selector
import Tepa.Http as Http
import Tepa.Mixin as Mixin exposing (Mixin)
import Tepa.Navigation as Nav
import Tepa.Scenario as Scenario exposing (Scenario)
import Tepa.Time as Time exposing (Posix)
import Test.Html.Event as HtmlEvent
import Test.Html.Event.Extra as HtmlEvent
import Test.Html.Query as Query
import Widget.Header as Header
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


{-| -}
leave : Promise Memory (Maybe Session)
leave =
    Tepa.currentState
        |> Tepa.map .session
        |> Tepa.map Just



-- View


{-| -}
view : Layer Memory -> Html
view =
    Tepa.layerView <|
        \context ->
            Html.div
                [ localClass "page"
                ]
                [ Header.view
                    (localClass "header")
                , Html.div
                    [ localClass "top"
                    ]
                    [ clockView (Tepa.mapViewContext .clock context)
                    , Html.div
                        [ localClass "greetingArea"
                        ]
                        [ Html.div
                            [ localClass "greetingArea_greeting"
                            ]
                            [ Html.span
                                [ localClass "greetingArea_greeting_text"
                                ]
                                [ Html.text "Hi, "
                                ]
                            , Html.span
                                [ localClass "greetingArea_greeting_name"
                                ]
                                [ Html.text context.state.session.profile.name
                                ]
                            , Html.span
                                [ localClass "greetingArea_greeting_text"
                                ]
                                [ Html.text "!"
                                ]
                            ]
                        , Html.div
                            [ localClass "greetingArea_luckyHay"
                            ]
                            [ Html.span
                                [ localClass "greetingArea_luckyHay_text"
                                ]
                                [ Html.text "Your lucky grass hay for today: "
                                ]
                            , Html.span
                                [ localClass "greetingArea_luckyHay_value"
                                ]
                                [ displayLuckyHay context.state.session.luckyHay
                                    |> Html.text
                                ]
                            ]
                        ]
                    ]
                , Html.div
                    [ localClass "body"
                    ]
                    [ editAccountFormView (Tepa.mapViewContext .editAccountForm context)
                    , Html.div
                        [ localClass "dashboard_links"
                        ]
                        [ Html.a
                            [ localClass "dashboard_links_linkButton-chat"
                            , Mixin.attribute "href"
                                (AppUrl.toString
                                    { path =
                                        [ Path.prefix
                                        , "chat"
                                        ]
                                    , queryParameters = Dict.empty
                                    , fragment = Nothing
                                    }
                                )
                            ]
                            [ Html.text "Start Chat"
                            ]
                        ]
                    , Toast.view
                        (Tepa.mapViewContext .toast context)
                    ]
                ]


{-| Stringify LuckyHay for display.
-}
displayLuckyHay : Session.LuckyHay -> String
displayLuckyHay hay =
    case hay of
        Session.LuckyHayTimothy ->
            "Timothy"

        Session.LuckyHayOat ->
            "Oat hay"

        Session.LuckyHayAlfalfa ->
            "Alfalfa"

        Session.LuckyHayOrchard ->
            "Orchard grass"

        Session.LuckyHayBermuda ->
            "Bermuda grass"


clockView : ViewContext ClockMemory -> Html
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


editAccountFormView : ViewContext EditAccountFormMemory -> Html
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
            [ localClass "editAccountForm_id"
            ]
            [ Html.div
                [ localClass "editAccountForm_id_label"
                ]
                [ Html.text "New name:"
                ]
            , Html.node "input"
                [ Mixin.attribute "type" "text"
                , Mixin.disabled state.isBusy
                , localClass "editAccountForm_id_input"
                , setKey EditAccount.keys.editAccountFormName
                , Mixin.boolAttribute "aria-invalid" <|
                    state.showError
                        && List.member EditAccount.IdRequired errors
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
            , Mixin.attribute "type" "button"
            , Mixin.boolAttribute "aria-busy" state.isBusy
            , Mixin.boolAttribute "aria-disabled" <| state.showError && not (List.isEmpty errors)
            , setKey keys.editAccountFormSaveButton
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
        [ clockProcedure
        , Tepa.bind Tepa.currentState <|
            \{ session } ->
                [ Tepa.setValue EditAccount.keys.editAccountFormName session.profile.name
                , editAccountFormProcedure bucket
                ]
        ]


clockProcedure : Promise Memory ()
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


editAccountFormProcedure : Bucket -> Promise Memory ()
editAccountFormProcedure bucket =
    -- IGNORE TCO
    let
        modifyEditAccountForm f =
            Tepa.modify <|
                \m ->
                    { m
                        | editAccountForm = f m.editAccountForm
                    }
    in
    Tepa.sequence
        [ Tepa.awaitViewEvent
            { key = keys.editAccountFormSaveButton
            , type_ = "click"
            }
        , modifyEditAccountForm <|
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
                                                                { profile | name = resp.name }
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
type alias ScenarioSet m =
    { layer : Layer m -> Maybe (Layer Memory)
    , changeEditAccountFormAccountId :
        { value : String
        }
        -> Scenario.Markup
        -> Scenario m
    , clickSubmitEditAccount :
        Scenario.Markup -> Scenario m
    , clickStartChat :
        Scenario.Markup -> Scenario m
    , receiveEditAccountResp :
        (Value -> Maybe ( Http.Metadata, String ))
        -> Scenario.Markup
        -> Scenario m
    , expectAvailable :
        Scenario.Markup -> Scenario m
    , expectEditAccountFormShowNoErrors :
        Scenario.Markup -> Scenario m
    , expectGreetingMessage :
        { value : String
        }
        -> Scenario.Markup
        -> Scenario m
    , expectClockMessage :
        { value : String
        }
        -> Scenario.Markup
        -> Scenario m
    , expectLuckyHayMessage :
        { value : String
        }
        -> Scenario.Markup
        -> Scenario m
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
scenario : ScenarioProps m -> ScenarioSet m
scenario props =
    { layer = props.querySelf
    , changeEditAccountFormAccountId = changeEditAccountFormAccountId props
    , clickSubmitEditAccount = clickSubmitEditAccount props
    , clickStartChat = clickStartChat props
    , receiveEditAccountResp = receiveEditAccountResp props
    , expectAvailable = expectAvailable props
    , expectEditAccountFormShowNoErrors = expectEditAccountFormShowNoErrors props
    , expectGreetingMessage = expectGreetingMessage props
    , expectClockMessage = expectClockMessage props
    , expectLuckyHayMessage = expectLuckyHayMessage props
    , editAccountEndpoint =
        { method = EditAccount.method
        , url = EditAccount.endpointUrl
        }
    }


changeEditAccountFormAccountId : ScenarioProps m -> { value : String } -> Scenario.Markup -> Scenario m
changeEditAccountFormAccountId props { value } markup =
    Scenario.userOperation props.session
        markup
        { query =
            Query.find
                [ localClassSelector "editAccountForm_id_input"
                ]
        , operation = HtmlEvent.change value
        }


clickSubmitEditAccount : ScenarioProps m -> Scenario.Markup -> Scenario m
clickSubmitEditAccount props markup =
    Scenario.userOperation props.session
        markup
        { query =
            Query.find
                [ localClassSelector "editAccountForm_saveButton"
                , Selector.attribute "aria-disabled" "false"
                , Selector.attribute "aria-busy" "false"
                ]
        , operation =
            HtmlEvent.click
        }


clickStartChat : ScenarioProps m -> Scenario.Markup -> Scenario m
clickStartChat props markup =
    let
        href =
            { path =
                [ Path.prefix
                , "chat"
                ]
            , queryParameters = Dict.empty
            , fragment = Nothing
            }
    in
    Scenario.sequence
        [ Scenario.expectAppView props.session
            (markup
                |> Scenario.modifyContent (String.append "(Expectation)")
                |> Scenario.hide True
            )
            { expectation =
                \{ body } ->
                    Query.fromHtml (Html.div [] body)
                        |> Query.find
                            [ localClassSelector "dashboard_links_linkButton-chat"
                            ]
                        |> Query.has
                            [ Selector.tag "a"
                            , Selector.attribute "href" <|
                                AppUrl.toString href
                            ]
            }
        , Scenario.pushPath props.session
            href
            markup
        ]


receiveEditAccountResp : ScenarioProps m -> (Value -> Maybe ( Http.Metadata, String )) -> Scenario.Markup -> Scenario m
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


expectAvailable : ScenarioProps m -> Scenario.Markup -> Scenario m
expectAvailable props markup =
    Scenario.expectMemory props.session
        markup
        { layer = props.querySelf
        , expectation = \_ -> Expect.pass
        }


expectEditAccountFormShowNoErrors : ScenarioProps m -> Scenario.Markup -> Scenario m
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
    -> Scenario m
expectGreetingMessage props { value } markup =
    Scenario.expectAppView props.session
        markup
        { expectation =
            \{ body } ->
                Query.fromHtml (Html.div [] body)
                    |> Query.find
                        [ localClassSelector "greetingArea_greeting"
                        ]
                    |> Query.has
                        [ localClassSelector "greetingArea_greeting_name"
                        , Selector.exactText value
                        ]
        }


expectClockMessage :
    ScenarioProps m
    ->
        { value : String
        }
    -> Scenario.Markup
    -> Scenario m
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


expectLuckyHayMessage :
    ScenarioProps m
    ->
        { value : String
        }
    -> Scenario.Markup
    -> Scenario m
expectLuckyHayMessage props { value } markup =
    Scenario.expectAppView props.session
        markup
        { expectation =
            \{ body } ->
                Query.fromHtml (Html.div [] body)
                    |> Query.find
                        [ localClassSelector "greetingArea_luckyHay"
                        ]
                    |> Query.has
                        [ localClassSelector "greetingArea_luckyHay_value"
                        , Selector.exactText value
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
    "page_home--"
