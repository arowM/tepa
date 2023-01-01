module Internal.MarkdownBuilder exposing
    ( Builder
    , run, root, break
    , Section
    , setSectionTitle
    , appendSectionChild
    , AppendMode
    , endAppendMode
    , appendParagraph
    , appendOrderedList
    , appendUnorderedList
    , appendCodeBlock
    , BlockElement
    , Paragraph
    , Inline
    , pushText
    , pushLink
    , pushCode
    , pushEmphasis
    , pushStrongEmphasis
    , pushStrikeThrough
    , OrderedList, OrderedListItem
    , appendOrderedListItem
    , appendParagraphToOrderedListItem
    -- , appendOrderedListToOrderedListItem
    -- , appendUnorderedListToOrderedListItem
    -- , appendCodeBlockToOrderedListItem
    , UnorderedList, CodeBlock
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

initAppendModeSection : AppendMode Section
initAppendModeSection =
    AppendMode
        { appendBlock = \elem (Section sec) ->
            Section
                { sec
                    | body = elem :: sec.body
                }
        , value =
            Section
                { title = ""
                , body = []
                , children = []
                }
        }


{-| -}
type BlockElement
    = ParagraphBlock Paragraph
    | OrderedListBlock OrderedList
    | UnorderedListBlock UnorderedList
    | CodeBlock CodeBlock


{-| -}
type Paragraph = Paragraph (List InlineElement) -- reversed

initInlineParagraph : Inline Paragraph
initInlineParagraph =
    Inline
        { push = \elem (Paragraph elems) ->
            Paragraph (elem :: elems)
        , value = Paragraph []
        }



{-| -}
type OrderedList = OrderedList (List OrderedListItem) -- reversed


{-| -}
type OrderedListItem =
    OrderedListItem
        { content : List InlineElement -- reversed
        , children : List BlockElement -- reversed
        }

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
    | InlineCode String
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
type Root = Root (AppendMode Section)


{-| -}
run : Builder parent a -> Section
run (Builder builder) =
    builder.current
        |> builder.parent
        |> builder.root
        |> (\(Root appendable) -> appendable)
        |> (\(AppendMode appendable) -> appendable.value)


{-| -}
root : Builder Root (AppendMode Section)
root = Builder
    { current = initAppendModeSection
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
setSectionTitle : String -> Builder parent (AppendMode Section) -> Builder parent (AppendMode Section)
setSectionTitle title (Builder builder) =
    Builder
        { builder
            | current =
                builder.current
                    |> modifyAppendMode
                        (\(Section sec) ->
                                    Section
                                        { sec | title = title }
                        )
        }


-- Append Mode

{-| -}
type AppendMode a = AppendMode
    { appendBlock : BlockElement -> a -> a
    , value : a
    }


modifyAppendMode : (a -> a) -> AppendMode a -> AppendMode a
modifyAppendMode f (AppendMode appendable) =
    AppendMode
        { appendable | value = f appendable.value }


append : BlockElement -> AppendMode a -> AppendMode a
append block (AppendMode appendable) =
    AppendMode
        { appendable
            | value =
                appendable.appendBlock block appendable.value
        }

{-| -}
appendParagraph :
   Builder parent (AppendMode a)
   -> Builder (Builder parent (AppendMode a)) Paragraph
appendParagraph builder =
    Builder
        { current = Paragraph []
        , parent = \para ->
            modify
                (append (ParagraphBlock para))
                builder
        , root = childRoot builder
        }


{-| -}
appendOrderedList :
   Builder parent (AppendMode a)
   -> Builder (Builder parent (AppendMode a)) OrderedList
appendOrderedList builder =
    Builder
        { current = OrderedList []
        , parent = \list ->
            modify
                (append (OrderedListBlock list))
                builder
        , root = childRoot builder
        }


{-| -}
appendUnorderedList :
   Builder parent (AppendMode a)
   -> Builder (Builder parent (AppendMode a)) UnorderedList
appendUnorderedList builder =
    Builder
        { current = UnorderedList []
        , parent = \list ->
            modify
                (append (UnorderedListBlock list))
                builder
        , root = childRoot builder
        }


{-| -}
appendCodeBlock :
   Builder parent (AppendMode a)
   -> Builder (Builder parent (AppendMode a)) CodeBlock
appendCodeBlock builder =
    Builder
        { current = CodeBlock_ []
        , parent = \code ->
            modify
                (append (CodeBlock code))
                builder
        , root = childRoot builder
        }


childRoot : Builder p b -> Builder p b -> Root
childRoot (Builder builder) =
    current >> builder.parent >> builder.root


modify : (a -> a) -> Builder parent a -> Builder parent a
modify f (Builder builder) =
    Builder
        { builder
            | current = f builder.current
        }

{-| -}
appendSectionChild :
   Builder parent (AppendMode Section)
   -> Builder (Builder parent (AppendMode Section)) (AppendMode Section)
appendSectionChild builder =
    Builder
        { current = initAppendModeSection
        , parent = \child ->
            modify
                (modifyAppendMode
                    (\(Section sec) ->
                        Section { sec | children = child :: sec.children }
                    )
                )
                builder
        , root = current >> builder.parent >> builder.root
        }


-- Inline

{-| -}
type Inline a = Inline
    { push : InlineElement -> a -> a
    , value : a
    }

modifyInline : (a -> a) -> Inline a -> Inline a
modifyInline f (Inline inline) =
    Inline
        { inline | value = f inline.value }

push : InlineElement -> Inline a -> Inline a
push elem (Inline inline) =
    Inline
        { inline
            | value =
                inline.push elem inline.value
        }

{-| -}
pushText : String -> Builder parent (Inline a) -> Builder parent (Inline a)
pushText str =
    modify <| push (PlainText str)


pushLink :
    { href : String
    , text : String
    }
    -> Builder parent (Inline a) -> Builder parent (Inline a)
pushLink r =
    modify <| push (Link r)


pushCode :
    String
    -> Builder parent (Inline a)
    -> Builder parent (Inline a)
pushCode str =
    modify <| push (InlineCode str)


pushEmphasis :
   String
   -> Builder parent (Inline a)
   -> Builder parent (Inline a)
pushEmphasis str =
    modify <| push (Emphasis str)

pushStrongEmphasis :
    String
    -> Builder parent (Inline a)
    -> Builder parent (Inline a)
pushStrongEmphasis str =
    modify <| push (StrongEmphasis str)


pushStrikeThrough :
   String
   -> Builder parent (Inline a)
   -> Builder parent (Inline a)
pushStrikeThrough str =
    modify <| push (Strikethrough str)


-- -- Ordered list


appendOrderedListItem : Builder parent OrderedList -> Builder (Builder parent OrderedList) OrderedListItem
appendOrderedListItem builder =
    Builder
        { current = OrderedListItem
            { content = []
            , children = []
            }
        , parent = \item ->
            modify
                ( \(OrderedList items) -> OrderedList (item :: items)
                )
                builder
        , root = childRoot builder
        }



{-| -}
appendParagraphToOrderedListItem : Builder parent OrderedListItem -> Builder (Builder parent OrderedListItem) Paragraph
appendParagraphToOrderedListItem builder =
    Builder
        { current = Paragraph []
        , parent = \para ->
            modify
                (\(OrderedListItem item) ->
                    OrderedListItem
                        { item
                            | children = ParagraphBlock para :: item.children
                        }
                )
                builder
        , root = childRoot builder
        }

{-| -}
appendOrderedListToOrderdListItem : Builder parent OrderedListItem -> Builder (Builder parent OrderedListItem) OrderedList
appendOrderedListToOrderdListItem builder =
    Builder
        { current = OrderedList []
        , parent = \list ->
            modify
                (\(OrderedListItem item) ->
                    OrderedListItem
                        { item
                            | children = OrderedListBlock list :: item.children
                        }
                )
                builder
        , root = childRoot builder
        }
-- 
-- 
-- {-| -}
-- appendUnorderedListBody : Builder parent Section -> Builder (Builder parent Section) UnorderedList
-- appendUnorderedListBody builder =
--     Builder
--         { current = UnorderedList []
--         , parent = \list ->
--             appendBodyBlock (UnorderedListBlock list) builder
--         , root = childRoot builder
--         }
-- 
-- 
-- {-| -}
-- appendCodeBlockBody : Builder parent Section -> Builder (Builder parent Section) CodeBlock
-- appendCodeBlockBody builder =
--     Builder
--         { current = CodeBlock_ []
--         , paent = \code ->
--             appendBodyBlock (CodeBlock code) builder
--         , root = childRoot builder
--         }
