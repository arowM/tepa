module Tepa.Scenario.Operation exposing
    ( Operation
    , custom
    , click
    , input
    , change
    , check
    , submit
    , blur
    , focus
    , doubleClick
    , mouseDown
    , mouseUp
    , mouseEnter
    , mouseLeave
    , mouseOver
    , mouseOut
    )

{-| Simulate user operations.


# Core

@docs Operation
@docs custom


# Common operations

@docs click
@docs input
@docs change
@docs check
@docs submit
@docs blur
@docs focus
@docs doubleClick
@docs mouseDown
@docs mouseUp
@docs mouseEnter
@docs mouseLeave
@docs mouseOver
@docs mouseOut

-}

import Internal.Core as Internal
import Json.Encode as JE exposing (Value)
import Test.Html.Event as TestEvent


{-| A simulated operation.
-}
type alias Operation event =
    Internal.Operation event


{-| Simulate a custom operation. The String is the event name, and the Value is the event object the browser would send to the event listener callback for the target HTML element.

    import Json.Encode as JE

    change : String -> Operation e
    change str =
        custom "change" <|
            JE.object
                [ ( "target"
                  , Encode.object
                        [ ( "value"
                          , JE.string str
                          )
                        ]
                  )
                ]

-}
custom : String -> Value -> Operation e
custom event val =
    TestEvent.custom event val
        |> Internal.customOperation


{-| A [`click`](https://developer.mozilla.org/en-US/docs/Web/Events/click) operation.
-}
click : Operation e
click =
    custom "click" <|
        JE.object []


{-| A [`dblclick`](https://developer.mozilla.org/en-US/docs/Web/Events/dblclick) operation.
-}
doubleClick : Operation e
doubleClick =
    custom "dblclick" <|
        JE.object []


{-| A [`mousedown`](https://developer.mozilla.org/en-US/docs/Web/Events/mousedown) operation.
-}
mouseDown : Operation e
mouseDown =
    custom "mousedown" <|
        JE.object []


{-| A [`mouseup`](https://developer.mozilla.org/en-US/docs/Web/Events/mouseup) operation.
-}
mouseUp : Operation e
mouseUp =
    custom "mouseup" <|
        JE.object []


{-| A [`mouseenter`](https://developer.mozilla.org/en-US/docs/Web/Events/mouseenter) operation.
-}
mouseEnter : Operation e
mouseEnter =
    custom "mouseenter" <|
        JE.object []


{-| A [`mouseleave`](https://developer.mozilla.org/en-US/docs/Web/Events/mouseleave) operation.
-}
mouseLeave : Operation e
mouseLeave =
    custom "mouseleave" <|
        JE.object []


{-| A [`mouseover`](https://developer.mozilla.org/en-US/docs/Web/Events/mouseover) operation.
-}
mouseOver : Operation e
mouseOver =
    custom "mouseover" <|
        JE.object []


{-| A [`mouseout`](https://developer.mozilla.org/en-US/docs/Web/Events/mouseout) operation.
-}
mouseOut : Operation e
mouseOut =
    custom "mouseout" <|
        JE.object []


{-| An [`input`](https://developer.mozilla.org/en-US/docs/Web/Events/input) operation.
-}
input : String -> Operation e
input value =
    custom "input" <|
        JE.object
            [ ( "target"
              , JE.object
                    [ ( "value"
                      , JE.string value
                      )
                    ]
              )
            ]


{-| An [`change`](https://developer.mozilla.org/en-US/docs/Web/Events/change) operation.
-}
change : String -> Operation e
change value =
    custom "change" <|
        JE.object
            [ ( "target"
              , JE.object
                    [ ( "value"
                      , JE.string value
                      )
                    ]
              )
            ]


{-| A [`change`](https://developer.mozilla.org/en-US/docs/Web/Events/change) operation
where `event.target.checked` is set to the given `Bool` value.
-}
check : Bool -> Operation e
check checked =
    custom "change" <|
        JE.object
            [ ( "target"
              , JE.object
                    [ ( "checked"
                      , JE.bool checked
                      )
                    ]
              )
            ]


{-| A [`submit`](https://developer.mozilla.org/en-US/docs/Web/Events/submit) operation.
-}
submit : Operation e
submit =
    custom "submit" <|
        JE.object []


{-| A [`blur`](https://developer.mozilla.org/en-US/docs/Web/Events/blur) operation.
-}
blur : Operation e
blur =
    custom "blur" <|
        JE.object []


{-| A [`focus`](https://developer.mozilla.org/en-US/docs/Web/Events/focus) operation.
-}
focus : Operation e
focus =
    custom "focus" <|
        JE.object []
