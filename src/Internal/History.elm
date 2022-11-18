module Internal.History exposing
    ( History
    , init
    , current
    , pushRoute
    , replaceRoute
    , back
    , forward
    )

{-| Emulator for browser history.

@docs History
@docs init
@docs current
@docs pushRoute
@docs replaceRoute
@docs back
@docs forward

-}


type alias Route =
    { path : String
    , query : Maybe String
    , fragment : Maybe String
    }


{-|

    entry : History
    entry =
        init
            { path = "/"
            , query = Nothing
            , fragment = Nothing
            }

    sample1 : Maybe History
    sample1 =
        entry
            |> replaceRoute
                { path = "/"
                , query = Nothing
                , fragment = Just "foo"
                }
            |> pushRoute
                { path = "/users"
                , query = Nothing
                , fragment = Nothing
                }
            |> back 1
            |> Maybe.andThen (forward 1)
            |> Maybe.map
                (replaceRoute
                    { path = "/users"
                    , query = Nothing
                    , fragment = Just "user-3"
                    }
                )
            |> Maybe.map
                (pushRoute
                    { path = "/user/3"
                    , query = Just "from=users"
                    , fragment = Nothing
                    }
                )


    sample1
        |> Maybe.andThen (back 1)
        |> Maybe.map current
    --> Just { path = "/users", query = Nothing, fragment = Just "user-3" }

    sample1
        |> Maybe.andThen (back 1)
        |> Maybe.andThen (back 1)
        |> Maybe.map current
    --> Just { path = "/", query = Nothing, fragment = Just "foo" }

    sample1
        |> Maybe.andThen (back 2)
        |> Maybe.andThen (forward 1)
        |> Maybe.map current
    --> Just { path = "/users", query = Nothing, fragment = Just "user-3" }

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
    { prev : List Route -- reversed
    , curr : Route
    , succ : List Route
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
pushRoute : Route -> History -> History
pushRoute route (History history) =
    History
        { prev = history.curr :: history.prev
        , curr = route
        , succ = []
        }


{-| -}
replaceRoute : Route -> History -> History
replaceRoute route (History history) =
    History
        { history
            | curr = route
        }


{-| -}
init : Route -> History
init route =
    History
        { prev = []
        , curr = route
        , succ = []
        }


{-| -}
current : History -> Route
current (History history) =
    history.curr
