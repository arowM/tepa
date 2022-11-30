module Internal.History exposing
    ( History
    , init
    , current
    , pushPath
    , replacePath
    , back
    , forward
    )

{-| Emulator for browser history.

@docs History
@docs init
@docs current
@docs pushPath
@docs replacePath
@docs back
@docs forward

-}

import Internal.AbsolutePath exposing (AbsolutePath)


{-|

    import Internal.AbsolutePath exposing (AbsolutePath(..))

    entry : History
    entry =
        init <|
            AbsolutePath
                { path = "/"
                , query = Nothing
                , fragment = Nothing
                }

    sample1 : Maybe History
    sample1 =
        entry
            |> replacePath
                ( AbsolutePath
                    { path = "/"
                    , query = Nothing
                    , fragment = Just "foo"
                    }
                )
            |> pushPath
                ( AbsolutePath
                    { path = "/users"
                    , query = Nothing
                    , fragment = Nothing
                    }
                )
            |> back 1
            |> Maybe.andThen (forward 1)
            |> Maybe.map
                (replacePath <|
                    AbsolutePath
                        { path = "/users"
                        , query = Nothing
                        , fragment = Just "user-3"
                        }
                )
            |> Maybe.map
                (pushPath <|
                    AbsolutePath
                        { path = "/user/3"
                        , query = Just "from=users"
                        , fragment = Nothing
                        }
                )


    sample1
        |> Maybe.andThen (back 1)
        |> Maybe.map current
    --> Just <| AbsolutePath
    -->     { path = "/users", query = Nothing, fragment = Just "user-3" }

    sample1
        |> Maybe.andThen (back 1)
        |> Maybe.andThen (back 1)
        |> Maybe.map current
    --> Just <| AbsolutePath
    -->     { path = "/", query = Nothing, fragment = Just "foo" }

    sample1
        |> Maybe.andThen (back 2)
        |> Maybe.andThen (forward 1)
        |> Maybe.map current
    --> Just <| AbsolutePath
    -->     { path = "/users", query = Nothing, fragment = Just "user-3" }

    sample1
        |> Maybe.andThen (back 3)
        |> Maybe.andThen (forward 1)
        |> Maybe.map current
    --> Nothing

    sample1
        |> Maybe.andThen (forward 1)
        |> Maybe.map current
    --> Nothing

-}
type History
    = History History_


type alias History_ =
    { prev : List AbsolutePath -- reversed
    , curr : AbsolutePath
    , succ : List AbsolutePath
    }


{-| -}
forward : Int -> History -> Maybe History
forward n =
    back (negate n)


{-| -}
back : Int -> History -> Maybe History
back n (History history) =
    if n == 0 then
        Just (History history)

    else if n > 0 then
        case history.prev of
            [] ->
                Nothing

            p :: ps ->
                back (n - 1) <|
                    History
                        { prev = ps
                        , curr = p
                        , succ = history.curr :: history.succ
                        }

    else
        case history.succ of
            [] ->
                Nothing

            s :: ss ->
                back (n + 1) <|
                    History
                        { prev = history.curr :: history.prev
                        , curr = s
                        , succ = ss
                        }


{-| -}
pushPath : AbsolutePath -> History -> History
pushPath path (History history) =
    History
        { prev = history.curr :: history.prev
        , curr = path
        , succ = []
        }


{-| -}
replacePath : AbsolutePath -> History -> History
replacePath path (History history) =
    History
        { history
            | curr = path
        }


{-| -}
init : AbsolutePath -> History
init path =
    History
        { prev = []
        , curr = path
        , succ = []
        }


{-| -}
current : History -> AbsolutePath
current (History history) =
    history.curr
