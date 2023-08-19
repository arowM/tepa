module Internal.Template exposing
    ( resolveInline
    , resolveBlock
    , Error(..)
    )

{-| Handle template strings.

@docs resolveInline
@docs resolveBlock
@docs Error

-}

import Dict exposing (Dict)


{-| Handle embeded parameters.

    import Dict


    resolveInline
        (Dict.fromList [ ("key", "bar") ])
        "foo{{key}}baz"
    --> Ok "foobarbaz"

    resolveInline
        (Dict.fromList [ ("key", "bar-2") ])
        "foo{{key}}baz"
    --> Ok "foobar\\-2baz"

    resolveInline
        (Dict.fromList [ ("key", "bar-2") ])
        "foo{{key|raw}}baz"
    --> Ok "foobar-2baz"

    resolveInline
        (Dict.fromList [ ("key2", "bar") ])
        "foo{{key}}baz"
    --> Err (ParameterNotFound "key")

    resolveInline
        (Dict.fromList [ ("key1", "bar"), ("key2", "baz") ])
        "foo{{key1}}{{key2}}"
    --> Ok "foobarbaz"

    resolveInline
        (Dict.fromList [ ("key1", "bar"), ("key2", "baz") ])
        "foo{{key1}}\\{{key2}}{{key2}}"
    --> Ok "foobar{{key2}}baz"

    resolveInline
        Dict.empty
        "{{key1|block}}"
    --> Ok "{{key1|block}}"

-}
resolveInline :
    Dict String String
    -> String
    -> Result Error String
resolveInline dict str =
    let
        reduced =
            String.foldr
                (\c context ->
                    case ( context, c ) of
                        ( AfterOneClosingBracket o, '}' ) ->
                            InBracket
                                { processed = o.processed
                                , key = ""
                                }

                        ( AfterOneClosingBracket o, _ ) ->
                            NoBrackets
                                { processed =
                                    String.cons '}' o.processed
                                        |> String.cons c
                                }

                        ( InBracket o, '{' ) ->
                            AfterOneOpeningBracket
                                { processed = o.processed
                                , key = o.key
                                }

                        ( InBracket o, '|' ) ->
                            if o.key == "block" then
                                NoBrackets
                                    { processed =
                                        "|block}}" ++ o.processed
                                    }

                            else
                                InBracket
                                    { processed = o.processed
                                    , key = String.cons c o.key
                                    }

                        ( InBracket o, _ ) ->
                            InBracket
                                { processed = o.processed
                                , key = String.cons c o.key
                                }

                        ( AfterOneOpeningBracket o, '{' ) ->
                            AfterTwoOpeningBracket o

                        ( AfterOneOpeningBracket o, '}' ) ->
                            AfterOneClosingBracket
                                { processed =
                                    "{" ++ o.key ++ "}}" ++ o.processed
                                }

                        ( AfterOneOpeningBracket o, _ ) ->
                            InBracket
                                { processed = o.processed
                                , key =
                                    String.cons '{' o.key
                                        |> String.cons c
                                }

                        ( AfterTwoOpeningBracket o, '\\' ) ->
                            NoBrackets
                                { processed =
                                    "{{" ++ o.key ++ "}}" ++ o.processed
                                }

                        ( AfterTwoOpeningBracket o, _ ) ->
                            let
                                raw =
                                    String.endsWith "|raw" o.key

                                key =
                                    if raw then
                                        String.dropRight 4 o.key

                                    else
                                        o.key
                            in
                            case Dict.get key dict of
                                Nothing ->
                                    HasError <|
                                        ParameterNotFound key

                                Just v ->
                                    let
                                        processed =
                                            (if raw then
                                                v

                                             else
                                                normalizePlainText v
                                            )
                                                ++ o.processed
                                    in
                                    if c == '}' then
                                        AfterOneClosingBracket
                                            { processed = processed
                                            }

                                    else
                                        NoBrackets
                                            { processed = String.cons c processed
                                            }

                        ( NoBrackets o, '}' ) ->
                            AfterOneClosingBracket
                                { processed = o.processed
                                }

                        ( NoBrackets o, _ ) ->
                            NoBrackets
                                { processed =
                                    String.cons c o.processed
                                }

                        ( HasError o, _ ) ->
                            HasError o
                )
                (NoBrackets
                    { processed = ""
                    }
                )
                str
    in
    case reduced of
        AfterOneClosingBracket o ->
            "}"
                ++ o.processed
                |> Ok

        InBracket o ->
            o.key
                ++ "}}"
                ++ o.processed
                |> Ok

        AfterOneOpeningBracket o ->
            "{"
                ++ o.key
                ++ "}}"
                ++ o.processed
                |> Ok

        AfterTwoOpeningBracket o ->
            let
                raw =
                    String.endsWith "|raw" o.key

                key =
                    if raw then
                        String.dropRight 4 o.key

                    else
                        o.key
            in
            case Dict.get key dict of
                Nothing ->
                    ParameterNotFound key
                        |> Err

                Just v ->
                    (if raw then
                        v

                     else
                        normalizePlainText v
                    )
                        ++ o.processed
                        |> Ok

        NoBrackets o ->
            o.processed
                |> Ok

        HasError err ->
            Err err


type Context
    = AfterOneClosingBracket
        { processed : String
        }
    | InBracket
        { processed : String
        , key : String
        }
    | AfterOneOpeningBracket
        { processed : String
        , key : String
        }
    | AfterTwoOpeningBracket
        { processed : String
        , key : String
        }
    | NoBrackets
        { processed : String
        }
    | HasError Error


{-| -}
type Error
    = ParameterNotFound String


normalizePlainText : String -> String
normalizePlainText rawStr =
    let
        hasHeadSpace =
            String.left 1 rawStr
                |> String.any isWhiteSpace

        hasLastSpace =
            String.right 1 rawStr
                -- `|> String.any isWhiteSpace` may cause infinite loop
                |> String.foldl
                    (\c acc ->
                        acc || isWhiteSpace c
                    )
                    False

        words =
            case normalizedWords rawStr of
                [] ->
                    []

                w :: ws ->
                    escapeDummyOrderedList w :: ws
    in
    List.concat
        [ if hasHeadSpace then
            [ "" ]

          else
            []
        , words
        , if not hasHeadSpace && hasLastSpace then
            [ "" ]

          else
            []
        ]
        |> String.join " "


escapeDummyOrderedList : String -> String
escapeDummyOrderedList str =
    if String.startsWith "." str then
        str

    else
        let
            extractResult o =
                Maybe.withDefault "" o.digits ++ o.acc
        in
        String.foldl
            (\c context ->
                case context.digits of
                    Nothing ->
                        if Char.isDigit c then
                            { acc = context.acc
                            , digits = Just <| String.fromChar c
                            }

                        else
                            { acc = String.cons c context.acc
                            , digits = Nothing
                            }

                    Just digits ->
                        if Char.isDigit c then
                            { acc = context.acc
                            , digits =
                                Just <| String.cons c digits
                            }

                        else if c == '.' then
                            { acc = ".\\" ++ digits ++ context.acc
                            , digits = Nothing
                            }

                        else
                            { acc = String.cons c digits ++ context.acc
                            , digits = Nothing
                            }
            )
            { acc = "" -- reversed
            , digits = Just "" -- reversed
            }
            str
            |> extractResult
            |> String.reverse


{-| Characters considered "whitespace" by `String.words`
-}
isWhiteSpace : Char -> Bool
isWhiteSpace c =
    List.member
        (Char.toCode c)
        [ 0x09
        , 0x0A
        , 0x0B
        , 0x0C
        , 0x0D
        , 0x20
        , 0x85
        , 0xA0
        , 0x1680
        , 0x2000
        , 0x2001
        , 0x2002
        , 0x2003
        , 0x2004
        , 0x2005
        , 0x2006
        , 0x2007
        , 0x2008
        , 0x2009
        , 0x200A
        , 0x2028
        , 0x2029
        , 0x202F
        , 0x205F
        , 0x3000
        ]


normalizedWords : String -> List String
normalizedWords =
    -- `String.words` does not recognize 0x0085 as whitespace.
    String.map
        (\c ->
            if Char.toCode c == 0x85 then
                ' '

            else
                c
        )
        -- Escape special characters
        >> String.foldr
            (\c acc ->
                if List.member c specialCharacters then
                    String.cons '\\' (String.cons c acc)

                else
                    String.cons c acc
            )
            ""
        >> String.words


specialCharacters : List Char
specialCharacters =
    [ '\\'
    , '`'
    , '*'
    , '_'
    , '{'
    , '}'
    , '['
    , ']'
    , '('
    , ')'
    , '#'
    , '+'
    , '-'
    , '!'
    , '>'
    , '~'
    ]


{-| Handle embedded blocks.

    import Dict

    resolveBlock
        (Dict.fromList
            [ ("foo", """{\n    "bar": "baz",\n    "foo": 3\n}""")
            ]
        )
        "{{foo|block}}  "
    --> Ok """{\n    "bar": "baz",\n    "foo": 3\n}"""

    resolveBlock
        (Dict.fromList
            [ ("foo", """{\n    "bar": "baz",\n    "foo": 3\n}""")
            ]
        )
        " {{foo|block}}"
    --> Ok " {{foo|block}}"

    resolveBlock
        Dict.empty
        "{{foo|block}}"
    --> Err (ParameterNotFound "foo")

-}
resolveBlock :
    Dict String String
    -> String
    -> Result Error String
resolveBlock dict raw =
    let
        str =
            String.trimRight raw
    in
    if
        String.startsWith "{{" str
            && String.endsWith "|block}}" str
    then
        let
            key =
                String.dropLeft 2 str |> String.dropRight 8
        in
        case Dict.get key dict of
            Nothing ->
                ParameterNotFound key
                    |> Err

            Just v ->
                Ok v

    else
        Ok raw
