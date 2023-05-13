module Widget.Toast exposing
    ( Memory
    , init
    , view
    , Event
    , Closed
    , pushWarning
    , pushError
    , pushHttpRequestError
    , scenario
    , ScenarioSet
    , ScenarioProps
    , toastTimeout
    , toastFadeOutDuration
    )

{-| Widget for toast popup.


# Core

@docs Memory
@docs init
@docs view
@docs Event
@docs Closed


# Methods

@docs pushWarning
@docs pushError


# Utilities

@docs pushHttpRequestError


# Scenario

@docs scenario
@docs ScenarioSet
@docs ScenarioProps


# Constants

@docs toastTimeout
@docs toastFadeOutDuration

-}

import App.ZIndex as ZIndex
import Expect
import Html.Attributes as Attributes
import Mixin exposing (Mixin)
import Mixin.Events as Events
import Mixin.Html as Html exposing (Html)
import Tepa exposing (Layer, Msg, Promise, Void)
import Tepa.Http as Http
import Tepa.Scenario as Scenario exposing (Scenario)
import Tepa.Scenario.LayerQuery as LayerQuery exposing (LayerQuery)
import Tepa.Time
import Test.Html.Query as HtmlQuery
import Test.Html.Selector as Selector



-- Constants


{-| How long it takes for the toast pop-up to start disappearing.
-}
toastTimeout : Int
toastTimeout =
    10000


{-| The duration of the effect for disappearing a toast item.
-}
toastFadeOutDuration : Int
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


{-| Represents that the popup is closed by user or timeout.
-}
type Closed
    = Closed



-- Methods


{-| Show warning message.
-}
pushWarning : String -> Promise Memory Event Closed
pushWarning =
    pushItem WarningMessage


{-| Show error message.
-}
pushError : String -> Promise Memory Event Closed
pushError =
    pushItem ErrorMessage


pushItem : MessageType -> String -> Promise Memory Event Closed
pushItem type_ str =
    Tepa.map (\_ -> Closed) <|
        Tepa.putNewLayer
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
            , init =
                { isHidden = False
                , messageType = type_
                , content = str
                }
            }
        <|
            \( newItemLayer, itemPointer ) ->
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



-- ToastItem


type alias ToastItemMemory =
    { isHidden : Bool
    , messageType : MessageType
    , content : String
    }


toastItemProcedure : Promise ToastItemMemory Event Void
toastItemProcedure =
    Tepa.sequence
        [ Tepa.withLayerEvent
            (\e ->
                case e of
                    CloseToastItem ->
                        [ Tepa.none
                        ]
            )
            |> Tepa.orFaster (Tepa.Time.sleep toastTimeout)
        , Tepa.modify
            (\m -> { m | isHidden = True })
        , Tepa.Time.sleep toastFadeOutDuration
        ]



-- Utilities


{-| Helper function to show HTTP request errors.
-}
pushHttpRequestError :
    Http.Response a
    -> Promise Memory Event Closed
pushHttpRequestError resp =
    case resp of
        Http.Timeout ->
            pushError """Network error, please try again."""

        Http.NetworkError ->
            pushError """Network error, please try again."""

        Http.BadUrl _ ->
            pushError """Internal error, please contact our support team."""

        Http.BadResponse _ _ ->
            pushError """Internal error, please contact our support team."""

        Http.GoodResponse _ _ ->
            Tepa.none
                |> Tepa.map (\_ -> Closed)



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
        , Mixin.style "--disappearing-duration" (String.fromInt toastFadeOutDuration ++ "ms")
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
type alias ScenarioSet flags m e =
    { expectWarningMessage :
        { message : String
        }
        -> Scenario.Markup
        -> Scenario flags m e
    , expectErrorMessage :
        { message : String
        }
        -> Scenario.Markup
        -> Scenario flags m e
    , expectDisappearingWarningMessage :
        { message : String
        }
        -> Scenario.Markup
        -> Scenario flags m e
    , expectDisappearingErrorMessage :
        { message : String
        }
        -> Scenario.Markup
        -> Scenario flags m e
    , expectNoWarningMessages : Scenario.Markup -> Scenario flags m e
    , expectNoErrorMessages : Scenario.Markup -> Scenario flags m e
    , expectNoMessages : Scenario.Markup -> Scenario flags m e
    , closeWarningsByMessage :
        { message : String
        }
        -> Scenario.Markup
        -> Scenario flags m e
    , closeErrorsByMessage :
        { message : String
        }
        -> Scenario.Markup
        -> Scenario flags m e
    }


{-| -}
type alias ScenarioProps m e =
    { querySelf : LayerQuery m Memory
    , wrapEvent : Event -> e
    , session : Scenario.Session
    }


{-| -}
scenario : ScenarioProps m e -> ScenarioSet flags m e
scenario props =
    { expectWarningMessage = expectMessage props WarningMessage
    , expectErrorMessage = expectMessage props ErrorMessage
    , expectDisappearingWarningMessage = expectDisappearingMessage props WarningMessage
    , expectDisappearingErrorMessage = expectDisappearingMessage props ErrorMessage
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
    , closeErrorsByMessage =
        closeByMessage props
            ErrorMessage
    }


expectMessage :
    ScenarioProps m e
    -> MessageType
    ->
        { message : String
        }
    -> Scenario.Markup
    -> Scenario flags m e
expectMessage props messageType { message } markup =
    Scenario.expectAppView props.session
        markup
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


expectDisappearingMessage :
    ScenarioProps m e
    -> MessageType
    ->
        { message : String
        }
    -> Scenario.Markup
    -> Scenario flags m e
expectDisappearingMessage props messageType { message } markup =
    Scenario.expectAppView props.session
        markup
        { expectation =
            \{ body } ->
                HtmlQuery.fromHtml (Html.div [] body)
                    |> HtmlQuery.findAll
                        [ localClassSelector <| "toast_item-" ++ messageTypeCode messageType
                        , Selector.attribute
                            (Attributes.attribute "aria-hidden" "true")
                        ]
                    |> HtmlQuery.keep
                        (Selector.all
                            [ localClassSelector "toast_item_body"
                            , Selector.text message
                            ]
                        )
                    |> HtmlQuery.count (Expect.greaterThan 0)
        }


expectNoMessages :
    ScenarioProps m e
    -> String
    -> Scenario.Markup
    -> Scenario flags m e
expectNoMessages props itemClassname markup =
    Scenario.expectAppView props.session
        markup
        { expectation =
            \{ body } ->
                HtmlQuery.fromHtml (Html.div [] body)
                    |> HtmlQuery.findAll
                        [ localClassSelector itemClassname
                        ]
                    |> HtmlQuery.count (Expect.equal 0)
        }


closeByMessage :
    ScenarioProps m e
    -> MessageType
    ->
        { message : String
        }
    -> Scenario.Markup
    -> Scenario flags m e
closeByMessage props messageType { message } markup =
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
    Scenario.sequence
        [ Scenario.layerEvent props.session
            markup
            { layer = target
            , event = props.wrapEvent CloseToastItem
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
