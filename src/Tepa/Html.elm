module Tepa.Html exposing
    ( Html
    , text
    , node
    , keyed
    , div
    , span
    , p
    , a
    , button
    , input
    , textarea
    , select
    , option
    )

{-| Quickly render HTML in Elm.


# Examples

The HTML part of a TEPA application looks something like this:

    import Tepa.Html as Html exposing (Html)
    import Tepa.Mixin as Mixin

    view : Layer Memory -> Html
    view =
        Tepa.layerView <|
            \{ state, setKey, values } ->
                Html.div []
                    [ Html.button
                        [ Mixin.attribute "type" "button"
                        , setKey "button-decrement"
                        ]
                        [ Html.text "-"
                        ]
                    , Html.div []
                        [ Html.text (String.fromInt state.count)
                        ]
                    , Html.button
                        [ Mixin.attribute "type" "button"
                        , setKey "button-increment"
                        ]
                        [ Html.text "+"
                        ]
                    ]

    type alias Memory =
        { count : Int
        }

If you call `view` you get something like this:

```html
<div>
  <button type="button">-</button>
  <div>42</div>
  <button type="button">+</button>
</div>
```


# Primitives

@docs Html
@docs text
@docs node
@docs keyed


# Super Common Tags

@docs div
@docs span
@docs p
@docs a
@docs button
@docs input
@docs textarea
@docs select
@docs option

-}

import Html
import Mixin.Html
import Tepa
import Tepa.Mixin exposing (Mixin)



-- Primitives


{-| The core building block used to build up HTML. Here we create an `Html`
value with no attributes and one child:

    hello : Html
    hello =
        div [] [ text "Hello!" ]

-}
type alias Html =
    Tepa.Html


{-| Just put plain text in the DOM. It will escape the string so that it appears
exactly as you specify.
-}
text : String -> Html
text =
    Html.text


{-| General way to create HTML nodes. It is used to define all of the helper
functions in this library.

    div : List Mixin -> List Html -> Html
    div mixins children =
        node "div" mixins children

You can use this to create custom nodes if you need to create something that
is not covered by the helper functions in this library.

The `Mixin` is bunch of attributes on your `Html`. Learn more in the
[`Tepa.Mixin`](Tepa-Mixin) module.

-}
node : String -> List Mixin -> List Html -> Html
node =
    Mixin.Html.node


{-| Works just like `node`, but you add a unique identifier to each child node.
You want this when you have a list of nodes that is changing: adding
nodes, removing nodes, etc. In these cases, the unique identifiers help make
the DOM modifications more efficient.

_This is the TEPA version of [`Html.Keyed.node`](https://package.elm-lang.org/packages/elm/html/latest/Html-Keyed#node)._

-}
keyed :
    String
    -> List Mixin
    -> List ( String, Html )
    -> Html
keyed =
    Mixin.Html.keyed


{-| Represents a generic container with no special meaning.

See [MDN document](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div) for details.

-}
div : List Mixin -> List Html -> Html
div =
    node "div"


{-| Represents text with no specific meaning.

See [MDN document](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span) for details.

-}
span : List Mixin -> List Html -> Html
span =
    node "span"


{-| Defines a portion that should be displayed as a paragraph.

See [MDN document](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p) for details.

-}
p : List Mixin -> List Html -> Html
p =
    node "p"


{-| Represents a hyperlink, linking to another resource.

See [MDN document](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a) for details.

-}
a : List Mixin -> List Html -> Html
a =
    node "a"


{-| Represents a button.

See [MDN document](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button) for details.

-}
button : List Mixin -> List Html -> Html
button =
    node "button"


{-| Represents a typed data field allowing the user to edit the data.

See [MDN document](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input) for details.

-}
input : List Mixin -> List Html -> Html
input =
    node "input"


{-| Represents a multiline text edit control.

See [MDN document](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea) for details.

-}
textarea : List Mixin -> List Html -> Html
textarea =
    node "textarea"


{-| Represents a control allowing selection among a set of options.

See [MDN document](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select) for details.

-}
select : List Mixin -> List Html -> Html
select =
    node "select"


{-| Represents an option in a `select` element or a suggestion of a `datalist`
element.

See [MDN document](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option) for details.

-}
option : List Mixin -> List Html -> Html
option =
    node "option"
