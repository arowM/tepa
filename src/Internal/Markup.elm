module Internal.Markup exposing
    ( BlockElement(..)
    , InlineElement(..)
    , Section(..)
    , toHtml
    )

import Mixin exposing (Mixin)
import Mixin.Html as Html exposing (Html)


{-| -}
type Section
    = Section
        { title : String
        , titleMixin : Mixin ()
        , body : List ( Mixin (), BlockElement )
        , bodyMixin : Mixin ()
        , children : List Section
        }


{-| -}
type BlockElement
    = Paragraph (List ( Mixin (), InlineElement ))
    | ListItems (Mixin ()) (List ( Mixin (), BlockElement ))
    | PreBlock (List ( Mixin (), InlineElement ))


{-| -}
type InlineElement
    = PlainText String
    | HyperLink
        { href : String
        , text : String
        }
    | InlineCode String
    | EmphasizedText String
    | StrongText String


toHtml : Section -> Html ()
toHtml =
    htmlSection 1


htmlSection : Int -> Section -> Html ()
htmlSection n (Section sec) =
    Html.div [ sec.bodyMixin ] <|
        htmlHeading n [ sec.titleMixin ] [ Html.text sec.title ]
            :: (List.map htmlBlockElement sec.body
                    ++ List.map (htmlSection (n + 1)) sec.children
               )


htmlHeading : Int -> List (Mixin ()) -> List (Html ()) -> Html ()
htmlHeading n mixins =
    if 1 <= n && n <= 6 then
        Html.node ("h" ++ String.fromInt n) mixins

    else
        Html.div
            [ Mixin.batch mixins
            , Mixin.attribute "role" "heading"
            , Mixin.attribute "aria-level" (String.fromInt n)
            ]


htmlBlockElement : ( Mixin (), BlockElement ) -> Html ()
htmlBlockElement ( mixin, elem ) =
    case elem of
        Paragraph inlines ->
            Html.p [ mixin ] <|
                List.map htmlInlineElement inlines

        ListItems liMixin blocks ->
            Html.node "ol" [ mixin ] <|
                List.map
                    (\block ->
                        Html.node "li"
                            [ liMixin ]
                            [ htmlBlockElement block
                            ]
                    )
                    blocks

        PreBlock inlines ->
            Html.node "pre" [ mixin ] <|
                List.map htmlInlineElement inlines


htmlInlineElement : ( Mixin (), InlineElement ) -> Html ()
htmlInlineElement ( mixin, elem ) =
    case elem of
        PlainText str ->
            Html.span [ mixin ] [ Html.text str ]

        HyperLink r ->
            Html.a
                [ mixin
                , Mixin.attribute "href" r.href
                ]
                [ Html.text r.text
                ]

        InlineCode str ->
            Html.node "code" [ mixin ] [ Html.text str ]

        EmphasizedText str ->
            Html.node "em" [ mixin ] [ Html.text str ]

        StrongText str ->
            Html.node "strong" [ mixin ] [ Html.text str ]
