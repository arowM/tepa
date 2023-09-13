module Tepa.HtmlSelector exposing
    ( Selector
    , tag, text, exactText, containing, attribute, boolAttribute, all
    , id, class, classes, exactClassName, disabled
    )

{-| Selecting HTML elements for testing.

_This is TEPA-friendly version of [Test.Html.Selector](https://package.elm-lang.org/packages/elm-explorations/test/latest/Test-Html-Selector)._

@docs Selector


## General Selectors

@docs tag, text, exactText, containing, attribute, boolAttribute, all


## Attributes

@docs id, class, classes, exactClassName, disabled

-}

import Html.Attributes as Attributes
import Test.Html.Selector as HtmlSelector


{-| A selector used to filter sets of elements.
-}
type alias Selector =
    HtmlSelector.Selector


{-| Combine the given selectors into one which requires all of them to match.

    import Tepa.Html
    import Tepa.Mixin as Mixin
    import Tepa.HtmlSelector exposing (class, text, all, Selector)
    import Test.Html.Query as Query
    import Test exposing (test)


    replyBtnSelector : Selector
    replyBtnSelector =
        all [ class "btn", text "Reply" ]


    test "Button has the class 'btn' and the text 'Reply'" <|
        \() ->
            Html.button [ Mixin.class "btn btn-large" ] [ Html.text "Reply" ]
                |> Query.fromHtml
                |> Query.has [ replyBtnSelector ]

-}
all : List Selector -> Selector
all =
    HtmlSelector.all


{-| Matches elements that have all the given classes (and possibly others as well).

When you only care about one class instead of several, you can use
[`class`](#class) instead of passing this function a list with one value in it.

To match the element's exact class attribute string, use [`exactClassName`](#exactClassName).

    import Tepa.Html
    import Tepa.Mixin as Mixin
    import Tepa.HtmlSelector exposing (classes)
    import Test.Html.Query as Query
    import Test exposing (test)


    test "Button has the classes btn and btn-large" <|
        \() ->
            Html.button [ Mixin.class "btn btn-large" ] [ Html.text "Reply" ]
                |> Query.fromHtml
                |> Query.has [ classes [ "btn", "btn-large" ] ]

-}
classes : List String -> Selector
classes =
    HtmlSelector.classes


{-| Matches elements that have the given class (and possibly others as well).

To match multiple classes at once, use [`classes`](#classes) instead.

To match the element's exact class attribute string, use [`exactClassName`](#exactClassName).

    import Tepa.Html
    import Tepa.Mixin as Mixin
    import Tepa.HtmlSelector exposing (class)
    import Test.Html.Query as Query
    import Test exposing (test)


    test "Button has the class btn-large" <|
        \() ->
            Html.button [ Mixin.class "btn btn-large" ] [ Html.text "Reply" ]
                |> Query.fromHtml
                |> Query.has [ class "btn-large" ]

-}
class : String -> Selector
class =
    HtmlSelector.class


{-| Matches the element's exact class attribute string.

This is used less often than [`class`](#class), [`classes`](#classes) or
[`attribute`](#attribute), which check for the _presence_ of a class as opposed
to matching the entire class attribute exactly.

    import Tepa.Html
    import Tepa.Mixin as Mixin
    import Tepa.HtmlSelector exposing (exactClassName)
    import Test.Html.Query as Query
    import Test exposing (test)


    test "Button has the exact class 'btn btn-large'" <|
        \() ->
            Html.button [ Mixin.class "btn btn-large" ] [ Html.text "Reply" ]
                |> Query.fromHtml
                |> Query.has [ exactClassName "btn btn-large" ]

-}
exactClassName : String -> Selector
exactClassName =
    HtmlSelector.exactClassName


{-| Matches elements that have the given `id` attribute.

    import Tepa.Html
    import Tepa.Mixin as Mixin
    import Tepa.HtmlSelector exposing (id, text)
    import Test.Html.Query as Query
    import Test exposing (test)


    test "the welcome <h1> says hello!" <|
        \() ->
            Html.div []
                [ Html.h1 [ Mixin.id "welcome" ] [ Html.text "Hello!" ] ]
                |> Query.fromHtml
                |> Query.find [ id "welcome" ]
                |> Query.has [ text "Hello!" ]

-}
id : String -> Selector
id =
    HtmlSelector.id


{-| Matches elements that have the given tag.

    import Tepa.Html
    import Tepa.Mixin as Mixin
    import Tepa.HtmlSelector exposing (tag, text)
    import Test.Html.Query as Query
    import Test exposing (test)


    test "the welcome <h1> says hello!" <|
        \() ->
            Html.div []
                [ Html.h1 [ Mixin.id "welcome" ] [ Html.text "Hello!" ] ]
                |> Query.fromHtml
                |> Query.find [ tag "h1" ]
                |> Query.has [ text "Hello!" ]

-}
tag : String -> Selector
tag =
    HtmlSelector.tag


{-| Matches elements that have the given attribute in a way that makes sense
given their semantics in `Html`.

Specify attribute name and its value as string. e.g.,:

    attribute "type_" "button"

-}
attribute : String -> String -> Selector
attribute key val =
    HtmlSelector.attribute (Attributes.attribute key val)


{-| Helper function for boolean attributes.

    boolAttribute key p =
        attribute key <|
            if p then
                "true"

            else
                "false"

-}
boolAttribute : String -> Bool -> Selector
boolAttribute key p =
    attribute key <|
        if p then
            "true"

        else
            "false"


{-| Matches elements that have a
[`text`](http://package.elm-lang.org/packages/elm-lang/html/latest/Html-Attributes#text)
attribute _containing_ the given value.

`Selector.text "11,22"` will match `Html.text "11,222"`.

If you need an exact match, take a look at [`exactText`](#exactText).

-}
text : String -> Selector
text =
    HtmlSelector.text


{-| Matches elements that have a
[`text`](http://package.elm-lang.org/packages/elm-lang/html/latest/Html-Attributes#text)
attribute with _exactly_ the given value (sans leading/trailing whitespace).

`Selector.exactText "11,22"` will _not_ match `Html.text "11,222"`.

Note this selector is whitespace sensitive (it _doesn't_ trim strings prior to
checking them):

`Selector.exactText "11,22"` will _not_ match `Html.text "\n    11,22   \n"`.

If you need a partial match, take a look at [`text`](#text).

-}
exactText : String -> Selector
exactText =
    HtmlSelector.exactText


{-| Matches elements whose descendants match the given selectors.

(You will get the element and **not** the descendant.)

This is especially useful to find elements which contain specific
text somewhere in their descendants.

    import Tepa.Html
    import Tepa.HtmlSelector exposing (class, containing, tag)
    import Tepa.Mixin as Mixin
    import Test exposing (test)
    import Test.Html.Event as Event
    import Test.Html.Query as Query

    test : Test
    test =
        test "..." <|
            Html.div []
                [ Html.button
                    [ Mixin.class "button-notMe"
                    ]
                    [ Html.text "not me" ]
                , Html.button
                    [ Mixin.class "button-clickMe"
                    ]
                    [ Html.text "click me" ]
                ]
                |> Query.fromHtml
                |> Query.find
                    [ tag "button"
                    , containing [ text "click me" ]
                    ]
                |> Query.has [ class "button-clickMe" ]

-}
containing : List Selector -> Selector
containing =
    HtmlSelector.containing


{-| Matches elements that have a
[`disabled`](http://package.elm-lang.org/packages/elm-lang/html/latest/Html-Attributes#disabled)
attribute with the given value.
-}
disabled : Bool -> Selector
disabled =
    HtmlSelector.disabled
