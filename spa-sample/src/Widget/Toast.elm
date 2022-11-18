module Widget.Toast exposing
    ( Memory
    , init
    , view
    , Event
    , Command
    , runCommand
    , Closed
    , pushWarning
    , pushError
    , pushHttpError
    , scenario
    , ScenarioSet
    , ScenarioProps
    )

{-| Widget for toast popup.


# Core

@docs Memory
@docs init
@docs view
@docs Event
@docs Command
@docs runCommand
@docs Closed


# Methods

@docs pushWarning
@docs pushError


# Utilities

@docs pushHttpError


# Scenario

@docs scenario
@docs ScenarioSet
@docs ScenarioProps

-}

import App.ZIndex as ZIndex
import Expect
import Http
import Mixin exposing (Mixin)
import Mixin.Events as Events
import Mixin.Html as Html exposing (Html)
import Process
import Task
import Tepa exposing (Layer, Msg, Promise, Void)
import Tepa.ResponseType as ResponseType
import Tepa.Scenario as Scenario exposing (Scenario)
import Tepa.Scenario.LayerQuery as LayerQuery exposing (LayerQuery)
import Test.Html.Query as HtmlQuery
import Test.Html.Selector as Selector



-- Constants


{-| How long it takes for the toast pop-up to disappear spontaneously in normal toasts.
-}
toastTimeout : Float
toastTimeout =
    10000


{-| The duration of the effect for disappearing a toast item.
-}
toastFadeOutDuration : Float
toastFadeOutDuration =
    250



-- Memory


{-| -}
type Memory
    = Memory Memory_


type alias Memory_ =
    { items : List (Layer ToastItemMemory)
    }


type MessageType
    = ErrorMessage
    | WarningMessage


messageTypeCode : MessageType -> String
messageTypeCode type_ =
    case type_ of
        ErrorMessage ->
            "error"

        WarningMessage ->
            "warning"


{-| -}
init : Memory
init =
    Memory
        { items = []
        }



-- Event


{-| -}
type Event
    = CloseToastItem


{-| -}
type Command
    = SetTimeoutOnItem (() -> Msg Event)
    | FadeOutItem (() -> Msg Event)


{-| -}
runCommand : Command -> Cmd (Msg Event)
runCommand cmd =
    case cmd of
        SetTimeoutOnItem toMsg ->
            Process.sleep toastTimeout
                |> Task.perform toMsg

        FadeOutItem toMsg ->
            Process.sleep toastFadeOutDuration
                |> Task.perform toMsg


{-| Represents that the popup is closed by user or timeout.
-}
type Closed
    = Closed



-- Methods


{-| Show warning message.
-}
pushWarning : String -> Promise Command Memory Event Closed
pushWarning =
    pushItem WarningMessage


{-| Show error message.
-}
pushError : String -> Promise Command Memory Event Closed
pushError =
    pushItem ErrorMessage


pushItem : MessageType -> String -> Promise Command Memory Event Closed
pushItem type_ str =
    let
        newItem =
            { isHidden = False
            , messageType = type_
            , content = str
            }
    in
    Tepa.newLayer
        { get =
            \getter (Memory m) ->
                List.filterMap getter m.items
                    |> List.head
        , modify =
            \modifier (Memory m) ->
                Memory
                    { m
                        | items = List.map modifier m.items
                    }
        }
        newItem
        |> Tepa.andThenSequence
            (\( newItemLayer, itemPointer ) ->
                [ Tepa.modify <|
                    \(Memory m) ->
                        Memory { m | items = m.items ++ [ newItemLayer ] }
                , toastItemProcedure
                    |> Tepa.onLayer itemPointer
                , Tepa.modify <|
                    \(Memory m) ->
                        Memory
                            { m
                                | items =
                                    List.filter
                                        (not << Tepa.isPointedBy itemPointer)
                                        m.items
                            }
                ]
            )
        |> Tepa.map (\_ -> Closed)



-- ToastItem


type alias ToastItemMemory =
    { isHidden : Bool
    , messageType : MessageType
    , content : String
    }


toastItemProcedure : Promise Command ToastItemMemory Event Void
toastItemProcedure =
    Tepa.sequence
        [ Tepa.withLayerEvent
            (\e ->
                case e of
                    CloseToastItem ->
                        [ Tepa.none
                        ]
            )
            |> Tepa.andRace
                (Tepa.customRequest
                    { name = "Set time out"
                    , request = SetTimeoutOnItem
                    , responseType = ResponseType.unit
                    }
                    |> Tepa.void
                )
        , Tepa.modify
            (\m -> { m | isHidden = True })
        , Tepa.customRequest
            { name = "fade out item"
            , request = FadeOutItem
            , responseType = ResponseType.unit
            }
            |> Tepa.void
        ]



-- Utilities


{-| Helper function to show HTTP errors.
-}
pushHttpError :
    Http.Error
    -> Promise Command Memory Event Closed
pushHttpError err =
    case err of
        Http.BadStatus 401 ->
            pushError """Incorrect ID or Password."""

        Http.BadStatus 403 ->
            pushError """Operation not permitted."""

        Http.Timeout ->
            pushError """Network error, please try again."""

        Http.NetworkError ->
            pushError """Network error, please try again."""

        _ ->
            pushError """Internal error, please contact our support team."""



-- View


{-| -}
view : Layer Memory -> Html (Msg Event)
view =
    Tepa.layerView <|
        \(Memory memory) ->
            Html.keyed "div"
                [ localClass "toast"
                , Mixin.style "--zindex" <| String.fromInt ZIndex.toast
                ]
                (List.map (Tepa.keyedLayerView toastItemView) memory.items)


toastItemView : ToastItemMemory -> Html (Msg Event)
toastItemView memory =
    Html.div
        [ localClass "toast_item"
        , localClass <| "toast_item-" ++ messageTypeCode memory.messageType
        , Mixin.attribute "role" "dialog"
        , Mixin.boolAttribute "aria-hidden" memory.isHidden
        , Mixin.style "--disappearing-duration" (String.fromFloat toastFadeOutDuration ++ "ms")
        ]
        [ Html.div
            [ localClass "toast_item_body"
            ]
            [ Html.text memory.content
            ]
        , Html.div
            [ localClass "toast_item_close"
            , Events.onClick CloseToastItem
                |> Tepa.eventMixin
            ]
            [ Html.text "×"
            ]
        ]



-- Scenario


{-| -}
type alias ScenarioSet flags c m e =
    { expectWarningMessage : { message : String } -> String -> Scenario flags c m e
    , expectErrorMessage : { message : String } -> String -> Scenario flags c m e
    , expectNoWarningMessages : String -> Scenario flags c m e
    , expectNoErrorMessages : String -> Scenario flags c m e
    , expectNoMessages : String -> Scenario flags c m e
    , closeWarningsByMessage : { message : String } -> Scenario flags c m e
    , closeErrorsByMessage : { message : String } -> Scenario flags c m e
    , awaitAllToDisappear : Scenario flags c m e
    }


{-| -}
type alias ScenarioProps c m e =
    { querySelf : LayerQuery m Memory
    , wrapEvent : Event -> e
    , unwrapCommand : c -> Maybe Command
    , session : Scenario.Session
    }


{-| -}
scenario : ScenarioProps c m e -> ScenarioSet flags c m e
scenario props =
    { expectWarningMessage = expectMessage props WarningMessage
    , expectErrorMessage = expectMessage props ErrorMessage
    , expectNoWarningMessages =
        expectNoMessages props
            ("toast_item-" ++ messageTypeCode WarningMessage)
    , expectNoErrorMessages =
        expectNoMessages props
            ("toast_item-" ++ messageTypeCode ErrorMessage)
    , expectNoMessages =
        expectNoMessages props
            "toast_item"
    , closeWarningsByMessage =
        closeByMessage props
            WarningMessage
            "Click close button on toast popup with warning message: "
    , closeErrorsByMessage =
        closeByMessage props
            ErrorMessage
            "Click close button on toast popup with error message: "
    , awaitAllToDisappear = awaitAllToDisappear props
    }


expectMessage : ScenarioProps c m e -> MessageType -> { message : String } -> String -> Scenario flags c m e
expectMessage props messageType { message } description =
    Scenario.expectAppView props.session
        description
        { expectation =
            \{ body } ->
                HtmlQuery.fromHtml (Html.div [] body)
                    |> HtmlQuery.findAll
                        [ localClassSelector <| "toast_item-" ++ messageTypeCode messageType
                        ]
                    |> HtmlQuery.keep
                        (Selector.all
                            [ localClassSelector "toast_item_body"
                            , Selector.text message
                            ]
                        )
                    |> HtmlQuery.count (Expect.greaterThan 0)
        }


expectNoMessages : ScenarioProps c m e -> String -> String -> Scenario flags c m e
expectNoMessages props itemClassname description =
    Scenario.expectAppView props.session
        description
        { expectation =
            \{ body } ->
                HtmlQuery.fromHtml (Html.div [] body)
                    |> HtmlQuery.findAll
                        [ localClassSelector itemClassname
                        ]
                    |> HtmlQuery.count (Expect.equal 0)
        }


closeByMessage : ScenarioProps c m e -> MessageType -> String -> { message : String } -> Scenario flags c m e
closeByMessage props messageType descPrefix { message } =
    let
        target =
            props.querySelf
                |> LayerQuery.children
                    (\(Memory m) -> m.items)
                |> LayerQuery.filter
                    (\m ->
                        m.messageType
                            == messageType
                            && m.content
                            == message
                    )
                |> LayerQuery.index 0
    in
    Scenario.concat
        [ Scenario.userEvent props.session
            (descPrefix ++ message)
            { target = target
            , event = props.wrapEvent CloseToastItem
            }
        , Scenario.customResponse props.session
            "The popup is gradually fading away."
            { target = target
            , response =
                \cmd ->
                    case props.unwrapCommand cmd of
                        Just (FadeOutItem toMsg) ->
                            toMsg ()
                                |> Tepa.mapMsg props.wrapEvent
                                |> Just

                        _ ->
                            Nothing
            }
        ]


awaitAllToDisappear : ScenarioProps c m e -> Scenario flags c m e
awaitAllToDisappear props =
    let
        targets =
            props.querySelf
                |> LayerQuery.children
                    (\(Memory m) -> m.items)
    in
    Scenario.concat
        [ Scenario.customResponse props.session
            "After a period of time, each popup are automatically removed."
            { target = targets
            , response =
                \cmd ->
                    case props.unwrapCommand cmd of
                        Just (SetTimeoutOnItem toMsg) ->
                            toMsg ()
                                |> Tepa.mapMsg props.wrapEvent
                                |> Just

                        _ ->
                            Nothing
            }
        , Scenario.customResponse props.session
            "Popups gradually fade away when removed."
            { target = targets
            , response =
                \cmd ->
                    case props.unwrapCommand cmd of
                        Just (FadeOutItem toMsg) ->
                            toMsg ()
                                |> Tepa.mapMsg props.wrapEvent
                                |> Just

                        _ ->
                            Nothing
            }
        ]



-- Helper functions


localClass : String -> Mixin msg
localClass name =
    Mixin.class (classPrefix ++ name)


localClassSelector : String -> Selector.Selector
localClassSelector name =
    Selector.class (classPrefix ++ name)


classPrefix : String
classPrefix =
    "widget_toast--"
