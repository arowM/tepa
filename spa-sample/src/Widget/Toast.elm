module Widget.Toast exposing
    ( Memory
    , init
    , view
    , ClosedBy(..)
    , pushWarning
    , pushError
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
@docs ClosedBy


# Methods

@docs pushWarning
@docs pushError


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
import Mixin.Html as Html exposing (Html)
import Tepa exposing (Layer, Msg, Promise)
import Tepa.Scenario as Scenario exposing (Scenario)
import Tepa.Stream as Stream
import Tepa.Time as Time
import Test.Html.Event as HtmlEvent
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
    350



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
init : Promise m Memory
init =
    Tepa.succeed <|
        Memory
            { items = []
            }


{-| Represents the reason why the popup is closed.

  - `ClosedByUser`: Uesr clicked the close button
  - `ClosedByTimeout`: The popup was timed out
  - `ClosedByLayer`: Layer has expired

-}
type ClosedBy
    = ClosedByUser
    | ClosedByTimeout
    | ClosedByLayer



-- Methods


{-| Show warning message.
This promise blocks subsequent processes untill the item is closed.
-}
pushWarning : String -> Promise Memory ClosedBy
pushWarning =
    pushItem WarningMessage


{-| Show error message.
This promise blocks subsequent processes untill the item is closed.
-}
pushError : String -> Promise Memory ClosedBy
pushError =
    pushItem ErrorMessage


pushItem : MessageType -> String -> Promise Memory ClosedBy
pushItem type_ str =
    Tepa.bindAndThen
        (Tepa.newLayer
            { isHidden = False
            , messageType = type_
            , content = str
            }
        )
    <|
        \newItem ->
            Tepa.modify
                (\(Memory m) ->
                    Memory { m | items = m.items ++ [ newItem ] }
                )
                |> Tepa.andThen
                    (\_ ->
                        toastItemProcedure
                            |> Tepa.onLayer
                                { get =
                                    \(Memory m) ->
                                        List.filter (Tepa.isOnSameLayer newItem) m.items
                                            |> List.head
                                , set =
                                    \new (Memory m) ->
                                        Memory
                                            { m
                                                | items =
                                                    List.map
                                                        (\item ->
                                                            if Tepa.isOnSameLayer newItem item then
                                                                new

                                                            else
                                                                item
                                                        )
                                                        m.items
                                            }
                                }
                    )
                |> Tepa.andThen
                    (\layerResult ->
                        case layerResult of
                            Tepa.LayerOk closedBy ->
                                Tepa.modify
                                    (\(Memory m) ->
                                        Memory
                                            { m | items = List.filter (not << Tepa.isOnSameLayer newItem) m.items }
                                    )
                                    |> Tepa.map (\_ -> closedBy)

                            _ ->
                                Tepa.succeed ClosedByLayer
                    )



-- ToastItem


type alias ToastItemMemory =
    { isHidden : Bool
    , messageType : MessageType
    , content : String
    }


toastItemProcedure : Promise ToastItemMemory ClosedBy
toastItemProcedure =
    Tepa.bindAndThen
        (Tepa.viewEventStream
            { key = keys.toastItemClose
            , type_ = "click"
            }
            |> Tepa.andThen
                (Stream.firstWithTimeout toastTimeout)
            |> Tepa.map
                (\ma ->
                    case ma of
                        Nothing ->
                            ClosedByTimeout

                        Just () ->
                            ClosedByUser
                )
        )
    <|
        \closedBy ->
            Tepa.sequence
                [ Tepa.modify
                    (\m -> { m | isHidden = True })
                , Time.sleep toastFadeOutDuration
                ]
                |> Tepa.map (\_ -> closedBy)



-- View


{-| -}
view : Tepa.ViewContext Memory -> Html Msg
view context =
    let
        (Memory state) =
            context.state
    in
    Html.keyed "div"
        [ localClass "toast"
        , Mixin.style "--zindex" <| String.fromInt ZIndex.toast
        ]
        (List.map (Tepa.keyedLayerView toastItemView) state.items)


toastItemView : Tepa.ViewContext ToastItemMemory -> Html Msg
toastItemView { state, setKey } =
    Html.div
        [ localClass "toast_item"
        , localClass <| "toast_item-" ++ messageTypeCode state.messageType
        , Mixin.attribute "role" "dialog"
        , Mixin.boolAttribute "aria-hidden" state.isHidden
        , Mixin.style "--disappearing-duration" (String.fromInt toastFadeOutDuration ++ "ms")
        ]
        [ Html.div
            [ localClass "toast_item_body"
            ]
            [ Html.text state.content
            ]
        , Html.div
            [ localClass "toast_item_close"
            , Mixin.fromAttributes
                (setKey keys.toastItemClose)
            ]
            [ Html.text "×"
            ]
        ]


keys :
    { toastItemClose : String
    }
keys =
    { toastItemClose = "toastItemClose"
    }



-- Scenario


{-| -}
type alias ScenarioSet flags m =
    { expectWarningMessage :
        { message : String
        }
        -> Scenario.Markup
        -> Scenario flags m
    , expectErrorMessage :
        { message : String
        }
        -> Scenario.Markup
        -> Scenario flags m
    , expectDisappearingWarningMessage :
        { message : String
        }
        -> Scenario.Markup
        -> Scenario flags m
    , expectDisappearingErrorMessage :
        { message : String
        }
        -> Scenario.Markup
        -> Scenario flags m
    , expectNoWarningMessages : Scenario.Markup -> Scenario flags m
    , expectNoErrorMessages : Scenario.Markup -> Scenario flags m
    , expectNoMessages : Scenario.Markup -> Scenario flags m
    , closeWarningsByMessage :
        { message : String
        }
        -> Scenario.Markup
        -> Scenario flags m
    , closeErrorsByMessage :
        { message : String
        }
        -> Scenario.Markup
        -> Scenario flags m
    }


{-| -}
type alias ScenarioProps m =
    { querySelf : Layer m -> Maybe (Layer Memory)
    , session : Scenario.Session
    }


{-| -}
scenario : ScenarioProps m -> ScenarioSet flags m
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
    ScenarioProps m
    -> MessageType
    ->
        { message : String
        }
    -> Scenario.Markup
    -> Scenario flags m
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
                            , Selector.exactText message
                            ]
                        )
                    |> HtmlQuery.count (Expect.greaterThan 0)
        }


expectDisappearingMessage :
    ScenarioProps m
    -> MessageType
    ->
        { message : String
        }
    -> Scenario.Markup
    -> Scenario flags m
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
                            , Selector.exactText message
                            ]
                        )
                    |> HtmlQuery.count (Expect.greaterThan 0)
        }


expectNoMessages :
    ScenarioProps m
    -> String
    -> Scenario.Markup
    -> Scenario flags m
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
    ScenarioProps m
    -> MessageType
    ->
        { message : String
        }
    -> Scenario.Markup
    -> Scenario flags m
closeByMessage props messageType { message } markup =
    Scenario.userOperation props.session
        markup
        { query =
            HtmlQuery.find
                [ localClassSelector "toast_item"
                , localClassSelector <|
                    "toast_item-"
                        ++ messageTypeCode messageType
                , Selector.attribute
                    (Attributes.attribute "aria-hidden" "false")
                , Selector.containing
                    [ localClassSelector "toast_item_body"
                    , Selector.exactText message
                    ]
                ]
                >> HtmlQuery.children
                    [ localClassSelector "toast_item_close"
                    ]
                >> HtmlQuery.first
        , operation = HtmlEvent.click
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
    "widget_toast--"
