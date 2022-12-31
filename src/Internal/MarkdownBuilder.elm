module Internal.MarkdownBuilder exposing
    ( Builder
    , run, root, break
    , Section
    , setTitle
    , appendChild
    , appendParagraphBody
    , appendOrderedListBody
    , appendUnorderedListBody
    , appendCodeBlock
    , BlockElement
    , Paragraph, OrderedList, UnorderedList, CodeBlock
    , InlineElement(..)
    )

{-| Build Markdown dynamically.
-}


{-| Main type representing Markdown section.
-}
type Section =
    Section
        { title : String
        , body : List BlockElement -- reversed
        , children : List Section -- reversed
        }

initSection : Section
initSection =
    Section
        { title = ""
        , body = []
        , children = []
        }


{-| -}
type BlockElement
    = ParagraphBlock Paragraph
    | OrderedListBlock OrderedList
    | UnorderedListBlock UnorderedList
    | CodeBlock CodeBlock


{-| -}
type Paragraph = Paragraph (List InlineElement)


{-| -}
type OrderedList = OrderedList (List BlockElement)


{-| -}
type UnorderedList = UnorderedList (List BlockElement)


{-| -}
type CodeBlock = CodeBlock_ (List InlineElement)


{-| -}
type InlineElement
    = PlainText String
    | Link
        { href : String
        , text : String
        }
    | Code String
    | Emphasis String
    | StrongEmphasis String
    | Strikethrough String


-- Builder Core

{-| -}
type Builder parent elem
    = Builder
        { current : elem
        , parent : elem -> parent
        , root : parent -> Root
        }


current : Builder parent a -> a
current (Builder builder) = builder.current

{-| -}
type Root = Root Section


{-| -}
run : Builder parent a -> Section
run (Builder builder) =
    builder.current
        |> builder.parent
        |> builder.root
        |> \(Root sec) -> sec

{-| -}
root : Builder Root Section
root = Builder
    { current = initSection
    , parent = Root
    , root = identity
    }


{-| -}
break : Builder (Builder parent child) a -> Builder parent child
break (Builder builder) =
    builder.parent builder.current


-- Modifiers

-- -- Section

{-| -}
setTitle : String -> Builder parent Section -> Builder parent Section
setTitle title (Builder builder) =
    Builder
        { builder
            | current =
                builder.current
                    |> \(Section sec) ->
                        Section
                            { sec | title = title }
        }


{-| -}
appendParagraphBody : Builder parent Section -> Builder (Builder parent Section) Paragraph
appendParagraphBody builder =
    Builder
        { current = Paragraph []
        , parent = \para ->
            appendBodyBlock (ParagraphBlock para) builder
        , root = childRoot builder
        }

{-| -}
appendOrderedListBody : Builder parent Section -> Builder (Builder parent Section) OrderedList
appendOrderedListBody builder =
    Builder
        { current = OrderedList []
        , parent = \list ->
            appendBodyBlock (OrderedListBlock list) builder
        , root = childRoot builder
        }


{-| -}
appendUnorderedListBody : Builder parent Section -> Builder (Builder parent Section) UnorderedList
appendUnorderedListBody builder =
    Builder
        { current = UnorderedList []
        , parent = \list ->
            appendBodyBlock (UnorderedListBlock list) builder
        , root = childRoot builder
        }


{-| -}
appendCodeBlock : Builder parent Section -> Builder (Builder parent Section) CodeBlock
appendCodeBlock builder =
    Builder
        { current = CodeBlock_ []
        , parent = \code ->
            appendBodyBlock (CodeBlock code) builder
        , root = childRoot builder
        }


childRoot : Builder p b -> Builder p b -> Root
childRoot (Builder builder) =
    current >> builder.parent >> builder.root


appendBodyBlock : BlockElement -> Builder parent Section -> Builder parent Section
appendBodyBlock block (Builder builder) =
    Builder
        { builder
            | current =
                builder.current
                    |> \(Section sec) ->
                        Section
                            { sec
                                | body = block :: sec.body
                            }
        }

{-| -}
appendChild : Builder parent Section -> Builder (Builder parent Section) Section
appendChild (Builder builder) =
    Builder
        { current = initSection
        , parent = \child ->
            Builder
                { builder
                    | current =
                        builder.current
                            |> \(Section sec) ->
                                Section { sec | children = child :: sec.children }
                }
        , root = current >> builder.parent >> builder.root
        }


-- -- Paragraph

