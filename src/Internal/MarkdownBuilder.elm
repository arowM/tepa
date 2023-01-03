module Internal.MarkdownBuilder exposing
    ( Builder
    , run
    , break
    , Root
    , root
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
    )

{-| This library helps your library or application to generate valid markdown document dynamically.


# Builder

@docs Builder
@docs break


# Root

@docs Root
@docs root
@docs run


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


## Push Mode

@docs PushMode
@docs endPushMode
@docs pushText
@docs pushLink
@docs pushCode
@docs pushEmphasis
@docs pushStrongEmphasis
@docs pushStrikethrough

-}


-- Internal


{-| -}
type BlockElement
    = ParagraphBlock Paragraph
    | ListBlock ListBlock
    | CodeBlock
        { head : String
        , body : String
        }


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
You can build a valid markdown structure dynamically in your program:

```elm
import Internal.MarkdownBuilder as MdBuilder

myMarkdown : MdBuilder.Builder
myMarkdown =
    MdBuilder.root
        { title = "Markdown Builder"
        }
        |> MdBuilder.editBody
        |> MdBuilder.appendParagraph
            "Build Markdown dynamically."
        |> MdBuilder.break
        |> MdBuilder.appendChildSection
            { title = "Builder"
            }
        |> MdBuilder.editBody
        |> MdBuilder.appendUnorderedList
        |> MdBuilder.appendListItem
        |> MdBuilder.editListItemContent
        |> MdBuilder.pushText "Builder"
        |> MdBuilder.endPushMode
        |> MdBuilder.editListItemChildren
        |> MdBuilder.appendCodeBlock
            { head = "elm"
            , body =
                """type Builder parent elem"""
            }
        |> MdBuilder.endAppendMode
        |> MdBuilder.run
```

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

```markdown
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
```
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


{-| Represents that `a` is in the *Append Mode*, which enalbes you to append some blocks to it.
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

The `head` property represents file name or file type followed by three backslashes, and the `body` property is for the code body.

````markdown
```(head)
First line in body.
Second line in body.
```
````
-}
appendCodeBlock :
    { head : String
    , body : String
    }
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


{-| Represents that `a` is in the *Push Mode*, which enables you to push some inline stuffs to it.
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
