module Internal.History exposing
    ( History
    , init
    , current
    , push
    , back
    , forward
    )

{-| Emulate browser history.

@docs History
@docs init
@docs current
@docs push
@docs back
@docs forward

-}

import AppUrl exposing (AppUrl)


{-| Emulator for browser history.
It only handles internal URLs.

    import Dict

    entry : History
    entry =
        init
            { path = []
            , queryParameters = Dict.empty
            , fragment = Nothing
            }


    sample1 : Maybe History
    sample1 =
        entry
            |> push
                { path = []
                , queryParameters = Dict.empty
                , fragment = Just "foo"
                }
            |> push
                { path = [ "users" ]
                , queryParameters = Dict.empty
                , fragment = Nothing
                }
            |> back 1
            |> Maybe.andThen (forward 1)
            |> Maybe.map
                (push
                    { path = [ "users" ]
                    , queryParameters = Dict.empty
                    , fragment = Just "user-3"
                    }
                )
            |> Maybe.map
                (push
                    { path = [ "user", "3" ]
                    , queryParameters =
                        Dict.fromList
                            [ ( "from", ["users"] )
                            ]
                    , fragment = Nothing
                    }
                )


    sample1
        |> Maybe.andThen (back 1)
        |> Maybe.map current
    --> Just
    -->     { path = [ "users" ]
    -->     , queryParameters = Dict.empty
    -->     , fragment = Just "user-3"
    -->     }

    sample1
        |> Maybe.andThen (back 1)
        |> Maybe.andThen (back 1)
        |> Maybe.map current
    --> Just
    -->     { path = [ "users" ]
    -->     , queryParameters = Dict.empty
    -->     , fragment = Nothing
    -->     }

    sample1
        |> Maybe.andThen (back 2)
        |> Maybe.andThen (forward 1)
        |> Maybe.map current
    --> Just
    -->     { path = [ "users" ]
    -->     , queryParameters = Dict.empty
    -->     , fragment = Just "user-3"
    -->     }

    sample1
        |> Maybe.andThen (back 5)
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
    { prev : List AppUrl -- reversed
    , curr : AppUrl
    , succ : List AppUrl
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
                back (n - 1)
                    (History
                        { prev = ps
                        , curr = p
                        , succ = history.curr :: history.succ
                        }
                    )

    else
        case history.succ of
            [] ->
                Nothing

            s :: ss ->
                back (n + 1)
                    (History
                        { prev = history.curr :: history.prev
                        , curr = s
                        , succ = ss
                        }
                    )


{-| Push new path into browser History.
-}
push : AppUrl -> History -> History
push path (History history) =
    History
        { prev = history.curr :: history.prev
        , curr = path
        , succ = []
        }


{-| -}
init : AppUrl -> History
init path =
    History
        { prev = []
        , curr = path
        , succ = []
        }


{-| Returns current path.
-}
current : History -> AppUrl
current (History history) =
    history.curr
