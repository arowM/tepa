module Internal.MarkdownBuilder exposing
    ( Builder
    , break
    , Root
    , root
    , run
    , toString
    , preview
    , Section
    , editBody
    , appendChildSection
    , ListBlock, ListItem
    , appendListItem
    , editListItemContent
    , editListItemChildren
    , AppendMode
    , endAppendMode
    , appendParagraph
    , Paragraph
    , appendOrderedList
    , appendUnorderedList
    , appendCodeBlock
    , PushMode
    , endPushMode
    , pushText
    , pushLink
    , pushCode
    , pushEmphasis
    , pushStrongEmphasis
    , pushStrikethrough
    , headTicks
    , inlineTicks
    , trimIndent
    )

{-| This library helps your library or application to generate valid markdown document programmatically.


# Builder

@docs Builder
@docs break


# Root

@docs Root
@docs root
@docs run
@docs toString
@docs preview


# Section

@docs Section
@docs editBody
@docs appendChildSection


# List Block

@docs ListBlock, ListItem
@docs appendListItem
@docs editListItemContent
@docs editListItemChildren


# General Modifiers


## Append Mode

@docs AppendMode
@docs endAppendMode
@docs appendParagraph
@docs Paragraph
@docs appendOrderedList
@docs appendUnorderedList
@docs appendCodeBlock

TODO: appendImage
TODO: appendQuote


## Push Mode

@docs PushMode
@docs endPushMode
@docs pushText
@docs pushLink
@docs pushCode
@docs pushEmphasis
@docs pushStrongEmphasis
@docs pushStrikethrough


## Internal

@docs headTicks
@docs inlineTicks
@docs trimIndent

-}

import Html exposing (Html)
import Html.Attributes as Attributes
import Url



-- Internal


{-| -}
type BlockElement
    = ParagraphBlock Paragraph
    | ListBlock ListBlock
    | CodeBlock String



-- | QuoteBlock
-- | ImageBlock


{-| -}
type Paragraph
    = Paragraph (List InlineElement) -- reversed


initParagraph : PushMode Paragraph
initParagraph =
    PushMode
        { push =
            \elem (Paragraph elems) ->
                Paragraph (elem :: elems)
        , value = Paragraph []
        }


{-| -}
type InlineElement
    = PlainText String
    | Link
        { href : String
        , text : String
        }
    | InlineCode String
    | Emphasis String
    | StrongEmphasis String
    | Strikethrough String



-- Builder


{-| Markdown builder.
You can build a valid markdown structure programmatically in your program:

    myMarkdown : Root
    myMarkdown =
        root
            { title = "Markdown Builder"
            }
            |> editBody
            |> appendParagraph
            |> pushText
                "Markdown Builder builds"
            |> pushEmphasis
                "Markdown"
            |> pushText
                "programmatically."
            |> break
            |> endAppendMode
            |> appendChildSection
                { title = "Builder"
                }
            |> editBody
            |> appendUnorderedList
            |> appendListItem
            |> editListItemContent
            |> pushText "List Item 1"
            |> endPushMode
            |> editListItemChildren
            |> appendOrderedList
            |> appendListItem
            |> editListItemContent
            |> pushText "Child item"
            |> break
            |> break
            |> break
            |> appendListItem
            |> editListItemContent
            |> pushText "List Item 2"
            |> endPushMode
            |> editListItemChildren
            |> appendParagraph
            |> pushText
                "Child paragraph."
            |> break
            |> appendCodeBlock
                """elm
                type Builder parent elem =
                    ...
                    ...
                """
            |> break
            |> appendListItem
            |> editListItemContent
            |> pushText "List Item 3"
            |> run

    toString myMarkdown
        |> String.lines
    --> [ "# Markdown Builder"
    --> , ""
    --> , "Markdown Builder builds *Markdown* programmatically."
    --> , ""
    --> , "## Builder"
    --> , ""
    --> , "* List Item 1"
    --> , "    1. Child item"
    --> , "* List Item 2"
    --> , ""
    --> , "    Child paragraph."
    --> , ""
    --> , "    ```elm"
    --> , "    type Builder parent elem ="
    --> , "        ..."
    --> , "        ..."
    --> , "    ```"
    --> , "* List Item 3"
    --> ]

-}
type Builder parent elem
    = Builder
        { current : elem
        , parent : elem -> parent
        , root : parent -> Root
        }


{-| Quit editing current child, and return to edit its parent.
-}
break : Builder (Builder parent child) a -> Builder parent child
break (Builder builder) =
    builder.parent builder.current


current : Builder parent a -> a
current (Builder builder) =
    builder.current


{-| Represents markdown root.
-}
type Root
    = Root Section


{-| Entry point for building markdown.
-}
root : { title : String } -> Builder Root Section
root { title } =
    Builder
        { current = initSection title
        , parent = Root
        , root = identity
        }


{-| Quit modifying markdown, and compile it to the valid markdown structure.
-}
run : Builder parent a -> Root
run (Builder builder) =
    builder.current
        |> builder.parent
        |> builder.root



-- Section


{-| Represents markdown section.

    # Title for Root Section

    Paragraph text in root section body.

    Another paragraph text in root section body.

    * List item in root section body
    * Another list item in root section body

    ## Title for Child Section

    Paragraph text in child section body.

    ```json
    {
      "message": "Code block in child section body."
    }
    ```

    ## Title for Another Child Section

    Paragraph text in another child section body.

    1. Ordered list item in another child section body
    1. Another ordered list item in another child section body

-}
type Section
    = Section
        { title : String
        , body : List BlockElement -- reversed
        , children : List Section -- reversed
        }


appendSectionBody : BlockElement -> Section -> Section
appendSectionBody elem (Section sec) =
    Section
        { sec
            | body = elem :: sec.body
        }


initSection : String -> Section
initSection title =
    Section
        { title = title
        , body = []
        , children = []
        }


{-| Start [Append Mode](#append-mode) for editing section body.
-}
editBody :
    Builder parent Section
    -> Builder parent (AppendMode Section)
editBody (Builder builder) =
    Builder
        { current =
            AppendMode
                { appendBlock = appendSectionBody
                , value = builder.current
                }
        , parent =
            \(AppendMode appendable) ->
                builder.parent appendable.value
        , root = builder.root
        }


{-| Append new child section to the current section, and change focus to the new one.
-}
appendChildSection :
    { title : String }
    -> Builder p Section
    -> Builder (Builder p Section) Section
appendChildSection { title } builder =
    Builder
        { current =
            initSection title
        , parent =
            \child ->
                modify
                    (\(Section sec) ->
                        Section { sec | children = child :: sec.children }
                    )
                    builder
        , root = childRoot builder
        }



-- List Block


{-| Represents markdown list block.
-}
type ListBlock
    = ListBlock_
        { ordered : Bool
        , items : List ListItem -- reversed
        }


{-| Represents an item in the `ListBlock`.

```markdown
* First `ListItem` content for this unordered `ListBlock`
    Child element for the first `ListItem`.

    1. This `ListItem` is in the child `ListBlock` for the First `ListItem` content.
    1. This `ListItem` is also in the child `ListBlock` for the First `ListItem` content.

* Second `ListItem` content for this unordered `ListBlock`
```

-}
type ListItem
    = ListItem
        { content : List InlineElement -- reversed
        , children : List BlockElement -- reversed
        }


{-| -}
appendListItem :
    Builder p ListBlock
    -> Builder (Builder p ListBlock) ListItem
appendListItem builder =
    Builder
        { current =
            ListItem
                { content = []
                , children = []
                }
        , parent =
            \listItem ->
                modify
                    (\(ListBlock_ listBlock_) ->
                        ListBlock_
                            { listBlock_
                                | items = listItem :: listBlock_.items
                            }
                    )
                    builder
        , root = childRoot builder
        }


{-| Start [Push Mode](#push-mode) for editing list item content.
-}
editListItemContent :
    Builder parent ListItem
    -> Builder parent (PushMode ListItem)
editListItemContent (Builder builder) =
    Builder
        { current =
            PushMode
                { push =
                    \elem (ListItem item) ->
                        ListItem
                            { item
                                | content = elem :: item.content
                            }
                , value = builder.current
                }
        , parent =
            \(PushMode inline) ->
                builder.parent inline.value
        , root = builder.root
        }


{-| Start [Append Mode](#append-mode) for editing list item children.
-}
editListItemChildren :
    Builder parent ListItem
    -> Builder parent (AppendMode ListItem)
editListItemChildren (Builder builder) =
    Builder
        { current =
            AppendMode
                { appendBlock = appendListItemChild
                , value = builder.current
                }
        , parent =
            \(AppendMode appendable) ->
                builder.parent appendable.value
        , root = builder.root
        }


appendListItemChild : BlockElement -> ListItem -> ListItem
appendListItemChild elem (ListItem item) =
    ListItem
        { item | children = elem :: item.children }



-- General Modifiers
-- -- Append Mode


{-| Represents that `a` is in the _Append Mode_, which enalbes you to append some blocks to it.
-}
type AppendMode a
    = AppendMode
        { appendBlock : BlockElement -> a -> a
        , value : a
        }


append : BlockElement -> AppendMode a -> AppendMode a
append block (AppendMode appendable) =
    AppendMode
        { appendable
            | value =
                appendable.appendBlock block appendable.value
        }


{-| End append mode.
-}
endAppendMode : Builder parent (AppendMode a) -> Builder parent a
endAppendMode (Builder builder) =
    let
        (AppendMode appendable) =
            builder.current
    in
    Builder
        { current = appendable.value
        , parent =
            \value ->
                builder.parent
                    (AppendMode { appendable | value = value })
        , root = builder.root
        }


{-| Append a paragraph block, and change to push mode for the paragraph.
-}
appendParagraph :
    Builder parent (AppendMode a)
    -> Builder (Builder parent (AppendMode a)) (PushMode Paragraph)
appendParagraph builder =
    Builder
        { current = initParagraph
        , parent =
            \(PushMode inline) ->
                modify
                    (append (ParagraphBlock inline.value))
                    builder
        , root = childRoot builder
        }


{-| Append an ordered list block, and change focus to it.
-}
appendOrderedList :
    Builder parent (AppendMode a)
    -> Builder (Builder parent (AppendMode a)) ListBlock
appendOrderedList builder =
    Builder
        { current =
            ListBlock_
                { ordered = True
                , items = []
                }
        , parent =
            \list ->
                modify
                    (append (ListBlock list))
                    builder
        , root = childRoot builder
        }


{-| Append an unordered list block, and change focus to it.
-}
appendUnorderedList :
    Builder parent (AppendMode a)
    -> Builder (Builder parent (AppendMode a)) ListBlock
appendUnorderedList builder =
    Builder
        { current =
            ListBlock_
                { ordered = False
                , items = []
                }
        , parent =
            \list ->
                modify
                    (append (ListBlock list))
                    builder
        , root = childRoot builder
        }


{-| Append a code block, and change focus to it.

Elm code:

    appendCodeBlock
        """elm
        appendCodeBlock
            = Debug.todo ""
        """

Generated markdown:

    ```elm
    appendCodeBlock
        = Debug.todo ""
    ```

-}
appendCodeBlock :
    String
    -> Builder parent (AppendMode a)
    -> Builder parent (AppendMode a)
appendCodeBlock r =
    modify
        (append (CodeBlock r))


childRoot : Builder p b -> Builder p b -> Root
childRoot (Builder builder) =
    current >> builder.parent >> builder.root


modify : (a -> a) -> Builder parent a -> Builder parent a
modify f (Builder builder) =
    Builder
        { builder
            | current = f builder.current
        }



-- -- Push Mode


{-| Represents that `a` is in the _Push Mode_, which enables you to push some inline stuffs to it.
-}
type PushMode a
    = PushMode
        { push : InlineElement -> a -> a
        , value : a
        }


{-| End push mode.
-}
endPushMode : Builder p (PushMode a) -> Builder p a
endPushMode (Builder builder) =
    let
        (PushMode inline) =
            builder.current
    in
    Builder
        { current = inline.value
        , parent =
            \value ->
                builder.parent
                    (PushMode { inline | value = value })
        , root = builder.root
        }


push : InlineElement -> PushMode a -> PushMode a
push elem (PushMode inline) =
    PushMode
        { inline
            | value =
                inline.push elem inline.value
        }


{-| Push a plain text.
-}
pushText : String -> Builder parent (PushMode a) -> Builder parent (PushMode a)
pushText str =
    modify <| push (PlainText str)


{-| Push a link.

```markdown
[href](text)
```

-}
pushLink :
    { href : String
    , text : String
    }
    -> Builder parent (PushMode a)
    -> Builder parent (PushMode a)
pushLink r =
    modify <| push (Link r)


{-| Push an inline code.

```markdown
`inline code`
```

-}
pushCode :
    String
    -> Builder parent (PushMode a)
    -> Builder parent (PushMode a)
pushCode str =
    modify <| push (InlineCode str)


{-| Push an emphasized text.

```markdown
*emphasized*
```

-}
pushEmphasis :
    String
    -> Builder parent (PushMode a)
    -> Builder parent (PushMode a)
pushEmphasis str =
    modify <| push (Emphasis str)


{-| Push a strongly emphasized text.

```markdown
**emphasized**
```

-}
pushStrongEmphasis :
    String
    -> Builder parent (PushMode a)
    -> Builder parent (PushMode a)
pushStrongEmphasis str =
    modify <| push (StrongEmphasis str)


{-| Push a strikethrough text.

```markdown
~~strikethrough~~
```

-}
pushStrikethrough :
    String
    -> Builder parent (PushMode a)
    -> Builder parent (PushMode a)
pushStrikethrough str =
    modify <| push (Strikethrough str)



-- Render to String


{-| Render `Root` into markdown text string.
-}
toString : Root -> String
toString (Root sec) =
    sectionToString
        { headerLevel = 1
        }
        sec


sectionToString : SectionStringContext -> Section -> String
sectionToString context (Section sec) =
    let
        childContext =
            { context | headerLevel = context.headerLevel + 1 }
    in
    List.concat
        [ [ String.join " "
                [ String.repeat context.headerLevel "#"
                , String.trim sec.title
                ]
          ]
        , List.reverse sec.body
            |> List.map
                (blockToString
                    { indentLevel = 0
                    }
                )
        , List.reverse sec.children
            |> List.map
                (sectionToString childContext)
        ]
        |> String.join "\n\n"


type alias SectionStringContext =
    { headerLevel : Int
    }


blockToString : BlockStringContext -> BlockElement -> String
blockToString context block =
    let
        setIndent : String -> String
        setIndent str =
            String.concat
                [ String.repeat context.indentLevel "    "
                , str
                ]
    in
    case block of
        ParagraphBlock (Paragraph ps) ->
            List.reverse ps
                |> List.map inlineToString
                |> String.join " "
                |> setIndent

        ListBlock (ListBlock_ param) ->
            List.reverse param.items
                |> List.map
                    (listItemToString param.ordered context)
                |> String.join "\n"

        CodeBlock param ->
            let
                ( header, codeLines ) =
                    case String.lines param of
                        [] ->
                            ( "", [] )

                        h :: bs ->
                            ( String.trim h
                            , trimIndent bs
                                |> List.take (List.length bs - 1)
                            )

                backslashes : Int
                backslashes =
                    List.map headTicks codeLines
                        |> List.maximum
                        |> Maybe.map
                            (\n -> n + 1)
                        |> Maybe.withDefault 3
                        |> max 3
            in
            String.join "\n"
                [ String.concat
                    [ String.repeat backslashes "`"
                    , String.trim header
                    ]
                    |> setIndent
                , List.map setIndent codeLines
                    |> String.join "\n"
                , String.repeat backslashes "`"
                    |> setIndent
                ]


{-|

    trimIndent
        [ "    foo bar"
        , "    bar baz baz"
        , "        and bar"
        ]
    --> [ "foo bar"
    --> , "bar baz baz"
    --> , "    and bar"
    --> ]

-}
trimIndent : List String -> List String
trimIndent ls =
    case ls of
        [] ->
            []

        s :: _ ->
            let
                indent =
                    headSpaces s
            in
            List.map (String.dropLeft indent) ls


headSpaces : String -> Int
headSpaces str =
    let
        folded =
            String.foldl
                (\c acc ->
                    Result.andThen
                        (\n ->
                            if c == ' ' then
                                -- May have another spaces
                                Ok (n + 1)

                            else
                                -- End of space sequence
                                Err n
                        )
                        acc
                )
                (Ok 0)
                str
    in
    case folded of
        Ok n ->
            n

        Err n ->
            n


{-| Count head ticks

    headTicks "\n ```foo"
    --> 3

    headTicks "foo```"
    --> 0

-}
headTicks : String -> Int
headTicks str =
    let
        folded =
            String.foldl
                (\c acc ->
                    Result.andThen
                        (\n ->
                            if c == '`' then
                                -- May have another backslashes
                                Ok (n + 1)

                            else
                                -- End of backslash sequence
                                Err n
                        )
                        acc
                )
                (Ok 0)
                (String.trim str)
    in
    case folded of
        Ok n ->
            n

        Err n ->
            n


type alias BlockStringContext =
    { indentLevel : Int
    }


listItemToString : Bool -> BlockStringContext -> ListItem -> String
listItemToString ordered context (ListItem item) =
    let
        symbol =
            if ordered then
                "1."

            else
                "*"

        setIndent : String -> String
        setIndent str =
            String.concat
                [ String.repeat context.indentLevel "    "
                , str
                ]

        children =
            List.reverse item.children

        hasBlankLine =
            case children of
                [] ->
                    False

                (ListBlock _) :: _ ->
                    False

                _ ->
                    True
    in
    List.concat
        [ [ String.join " "
                [ symbol
                , List.reverse item.content
                    |> List.map inlineToString
                    |> String.join " "
                ]
                |> setIndent
          ]
        , if hasBlankLine then
            [ "" ]

          else
            []
        , children
            |> List.map
                (blockToString
                    { context | indentLevel = context.indentLevel + 1 }
                )
            |> List.intersperse ""
        ]
        |> String.join "\n"


inlineToString : InlineElement -> String
inlineToString inline =
    case inline of
        PlainText text ->
            String.trim text
                |> String.lines
                |> String.join " "

        Link param ->
            String.concat
                [ "["
                , String.trim param.text
                    |> String.lines
                    |> String.join " "
                , "]("
                , param.href
                , ")"
                ]

        InlineCode code ->
            let
                backslashes =
                    inlineTicks code + 1
            in
            String.concat
                [ String.repeat backslashes "`"
                , code
                , String.repeat backslashes "`"
                ]

        Emphasis str ->
            String.concat
                [ "*"
                , String.trim str
                    |> String.lines
                    |> String.join " "
                , "*"
                ]

        StrongEmphasis str ->
            String.concat
                [ "**"
                , String.trim str
                    |> String.lines
                    |> String.join " "
                , "**"
                ]

        Strikethrough str ->
            String.concat
                [ "~~"
                , String.trim str
                    |> String.lines
                    |> String.join " "
                , "~~"
                ]


{-|

    inlineTicks "foo`bar``baz"
    --> 2

    inlineTicks "foobarbaz"
    --> 0

-}
inlineTicks : String -> Int
inlineTicks str =
    let
        ( n1, n2 ) =
            String.foldl
                (\c ( best, curr ) ->
                    if c == '`' then
                        ( best, curr + 1 )

                    else
                        ( max best curr, 0 )
                )
                ( 0, 0 )
                str
    in
    max n1 n2



-- Preview


{-| Preview markdown content as an HTML page.
-}
preview : Root -> Html msg
preview (Root sec) =
    previewSection
        { headerLevel = 1
        }
        sec


previewSection : SectionPreviewContext -> Section -> Html msg
previewSection context (Section sec) =
    List.concat
        [ [ previewHeader context.headerLevel sec.title
          ]
        , List.reverse sec.body
            |> List.map previewBlock
        , List.reverse sec.children
            |> List.map
                (previewSection
                    { context
                        | headerLevel =
                            context.headerLevel + 1
                    }
                )
        ]
        |> Html.div []


type alias SectionPreviewContext =
    { headerLevel : Int
    }


previewHeader : Int -> String -> Html msg
previewHeader n str =
    let
        attr =
            [ Attributes.id <| Url.percentEncode str
            ]
    in
    case n of
        1 ->
            Html.h1 attr [ Html.text str ]

        2 ->
            Html.h2 attr [ Html.text str ]

        3 ->
            Html.h3 attr [ Html.text str ]

        4 ->
            Html.h4 attr [ Html.text str ]

        5 ->
            Html.h5 attr [ Html.text str ]

        6 ->
            Html.h6 attr [ Html.text str ]

        _ ->
            Html.div attr [ Html.text str ]


previewBlock : BlockElement -> Html msg
previewBlock block =
    case block of
        ParagraphBlock (Paragraph ps) ->
            List.reverse ps
                |> List.map previewInline
                |> Html.p []

        ListBlock (ListBlock_ param) ->
            let
                container =
                    if param.ordered then
                        Html.ol

                    else
                        Html.ul
            in
            List.reverse param.items
                |> List.map previewListItem
                |> container []

        CodeBlock param ->
            let
                ( header, codeLines ) =
                    case String.lines param of
                        [] ->
                            ( "", [] )

                        h :: bs ->
                            ( String.trim h
                            , trimIndent bs
                                |> List.take (List.length bs - 1)
                            )
            in
            List.map Html.text codeLines
                |> Html.code [ Attributes.attribute "data-code-header" header ]
                |> List.singleton
                |> Html.pre []


previewListItem : ListItem -> Html msg
previewListItem (ListItem item) =
    List.concat
        [ [ List.reverse item.content
                |> List.map previewInline
                |> Html.p []
          ]
        , List.reverse item.children
            |> List.map previewBlock
        ]
        |> Html.li []


previewInline : InlineElement -> Html msg
previewInline inline =
    case inline of
        PlainText text ->
            Html.text text

        Link param ->
            Html.a
                [ Attributes.href param.href
                ]
                [ Html.text param.text
                ]

        InlineCode code ->
            Html.code [] [ Html.text code ]

        Emphasis text ->
            Html.em [] [ Html.text text ]

        StrongEmphasis text ->
            Html.strong [] [ Html.text text ]

        Strikethrough text ->
            Html.del [] [ Html.text text ]
