module Tepa.Mixin exposing
    ( Mixin
    , batch
    , none
    , style
    , attribute
    , class
    , id
    , disabled
    , boolAttribute
    , when
    , unless
    , withMaybe
    )

{-|


# Primitives

@docs Mixin
@docs batch
@docs none
@docs style
@docs attribute


# Super Common Attributes

@docs class
@docs id


# Boolean Attributes

@docs disabled
@docs boolAttribute


# Conditional Functions

@docs when
@docs unless
@docs withMaybe

-}

import Mixin
import Tepa



-- Core


{-| `Mixin` is a set of HTML attributes.
-}
type alias Mixin =
    Tepa.Mixin


{-| No HTML attributes.
-}
none : Mixin
none =
    Mixin.none


{-| When you need to set a couple HTML attributes only if a certain condition is met, you can batch them together.

    import Tepa.Html as Html exposing (Html)
    import Tepa.Mixin as Mixin

    greeting : Animal -> Html
    greeting animal =
        Html.div
            [ Mixin.class "greeting"
            , case animal of
                Goat { horns } ->
                    Mixin.batch
                        [ Mixin.class "greeting-goat"

                        -- CSS custom properties
                        , Mixin.style "--horns" (String.fromInt horns)
                        ]

                Dog ->
                    Mixin.batch
                        [ Mixin.class "greeting-dog"
                        ]

                _ ->
                    Mixin.none
            ]
            [ text "Hello!"
            ]

Note1: `Mixin.none` and `Mixin.batch [ Mixin.none, Mixin.none ]` and `Mixin.batch []` all do the same thing.

Note2: It simply flattens as each item appears; so `[ Mixin.batch [ foo, bar ], baz, Mixin.batch [ foobar, foobaz ] ]` is reduced to `[ foo, bar, baz, foobar, foobaz ]`.

-}
batch : List Mixin -> Mixin
batch =
    Mixin.batch


{-| Specify a style.

    import Tepa.Html as Html exposing (Html)
    import Tepa.Mixin as Mixin

    greeting : Html
    greeting =
        Html.div
            [ Mixin.style "background-color" "red"
            , Mixin.style "height" "90px"
            , Mixin.style "--min-height" "3em"
            , Mixin.style "width" "100%"
            ]
            [ text "Hello!"
            ]

This `style` can also handle [CSS custom properties](https://developer.mozilla.org/docs/Web/CSS/Using_CSS_custom_properties) well.\_

-}
style : String -> String -> Mixin
style =
    Mixin.style


{-| Create attributes, like saying domNode.setAttribute('class', 'greeting') in JavaScript.

    class : String -> Mixin
    class name =
        Mixin.attribute "class" name

-}
attribute : String -> String -> Mixin
attribute =
    Mixin.attribute



-- Super Common Attributes


{-| Often used with CSS to style elements with common properties.

Note: You can have as many `class` attributes as you want. They all get applied, so if you say `[ class "notice", class "notice-seen" ]` you will get both classes!

-}
class : String -> Mixin
class =
    Mixin.class


{-| Often used with CSS to style a specific element. The value of this attribute must be unique.
-}
id : String -> Mixin
id =
    Mixin.id


{-| Indicates whether the user can interact with a `button`, `fieldset`, `input`, `optgroup`, `option`, `select` or `textarea`.
-}
disabled : Bool -> Mixin
disabled =
    Mixin.disabled


{-| Create arbitrary bool `attribute`.
The `boolAttribute` converts the `Bool` argument into the string `"true"` or `"false"`.

    ariaHidden : Bool -> Mixin
    ariaHidden =
        boolAttribute "aria-hidden"

-}
boolAttribute : String -> Bool -> Mixin
boolAttribute =
    Mixin.boolAttribute



-- Conditional functions


{-| Insert a `Mixin` only when conditions are met.
-}
when : Bool -> Mixin -> Mixin
when =
    Mixin.when


{-| Insert a `Mixin` unless conditions are met.
-}
unless : Bool -> Mixin -> Mixin
unless =
    Mixin.unless


{-| Insert a `Mixin` only when the value actually exists.
-}
withMaybe : Maybe a -> (a -> Mixin) -> Mixin
withMaybe =
    Mixin.withMaybe
