module Tepa.Stream exposing
    ( Stream
    , awaitFirst, awaitFirstWithTimeout
    , awaitAll, awaitAllWithTimeout
    , awaitWhile, awaitUnless
    , run
    , while
    , map
    , filter
    , filterMap
    , take
    , union
    , scan
    , oneOf, Case, continue, break, customCase
    )

{-| Handle data stream.


# Stream

@docs Stream
@docs awaitFirst, awaitFirstWithTimeout
@docs awaitAll, awaitAllWithTimeout
@docs awaitWhile, awaitUnless
@docs run
@docs while
@docs map
@docs filter
@docs filterMap
@docs take
@docs union
@docs scan


# Handle cases

@docs oneOf, Case, continue, break, customCase

-}

import Internal.Core as Core
import Tepa exposing (Promise)


{-| `Stream` is a _specification_ of how to retrieve a sequence of data arriving one after another in real time until it ends.

Since it is only a _specification_, it is only when it is evaluated by `run` or await functions that your actually start getting data.

-}
type alias Stream a =
    Core.Stream a


{-| -}
map : (a -> b) -> Stream a -> Stream b
map f stream =
    case stream of
        Core.ActiveStream param ->
            Core.ActiveStream
                { unwrapMsg =
                    \msg ->
                        let
                            ( data, next ) =
                                param.unwrapMsg msg
                        in
                        ( List.map f data
                        , map f next
                        )
                , dependencies = param.dependencies
                , released = param.released
                }

        Core.EndOfStream param ->
            Core.EndOfStream param


{-| -}
filter : (a -> Bool) -> Stream a -> Stream a
filter p =
    filterMap
        (\a ->
            if p a then
                Just a

            else
                Nothing
        )


{-| -}
filterMap : (a -> Maybe b) -> Stream a -> Stream b
filterMap f stream =
    case stream of
        Core.ActiveStream param ->
            Core.ActiveStream
                { unwrapMsg =
                    \msg ->
                        let
                            ( data, next ) =
                                param.unwrapMsg msg
                        in
                        ( List.filterMap f data
                        , filterMap f next
                        )
                , dependencies = param.dependencies
                , released = param.released
                }

        Core.EndOfStream param ->
            Core.EndOfStream param


{-| Take first N data from the stream, and close the Stream.

    import Tepa exposing (Promise)
    import Tepa.Stream as Stream

    sample : Promise m ()
    sample =
        Stream.bind
            (Tepa.httpRequest options
                |> Stream.take 1
            )
        <|
            \response ->
                []

-}
take : Int -> Stream a -> Stream a
take n stream =
    case stream of
        Core.ActiveStream param ->
            Core.ActiveStream
                { unwrapMsg =
                    \msg ->
                        let
                            ( data, next ) =
                                param.unwrapMsg msg

                            dataCount =
                                List.length data
                        in
                        if dataCount < n then
                            ( data
                            , take (n - dataCount) next
                            )

                        else
                            ( List.take n data
                            , cancel next
                            )
                , dependencies = param.dependencies
                , released = param.released
                }

        Core.EndOfStream param ->
            Core.EndOfStream param


{-| Combine two streams.

    import Json.Decoder as JD
    import Tepa exposing (Promise)
    import Tepa.Stream as Stream

    type WithTimeout
        = TimedOut
        | ClickSubmitButton

    withTimeout : Promise m WithTimeout
    withTimeout =
        Tepa.customViewEvent
            { key = "submit"
            , type_ = "click"
            , decoder =
                JD.succeedPromise
                    { stopPropagation = False
                    , preventDefault = False
                    , value = ClickSubmitButton
                    }
            }
            |> Stream.union
                (Time.periodic 3000
                    |> Tepa.map
                        (Stream.map (\_ -> TimedOut))
                )
            |> Stream.first

-}
union : List (Stream a) -> Stream a
union =
    List.foldl merge
        (Core.EndOfStream
            { released = []
            }
        )


merge : Stream a -> Stream a -> Stream a
merge s1 s2 =
    case ( s1, s2 ) of
        ( Core.EndOfStream param1, Core.EndOfStream param2 ) ->
            Core.EndOfStream
                { released = param1.released ++ param2.released
                }

        ( Core.ActiveStream param1, Core.EndOfStream param2 ) ->
            Core.ActiveStream
                { param1
                    | released = param1.released ++ param2.released
                }

        ( Core.EndOfStream param1, Core.ActiveStream param2 ) ->
            Core.ActiveStream
                { param2
                    | released = param1.released ++ param2.released
                }

        ( Core.ActiveStream param1, Core.ActiveStream param2 ) ->
            Core.ActiveStream
                { unwrapMsg =
                    \msg ->
                        let
                            ( data1, next1 ) =
                                param1.unwrapMsg msg

                            ( data2, next2 ) =
                                param2.unwrapMsg msg
                        in
                        ( data1 ++ data2
                        , merge next1 next2
                        )
                , dependencies = param1.dependencies ++ param2.dependencies
                , released = param1.released ++ param2.released
                }


cancel : Stream a -> Stream b
cancel stream =
    case stream of
        Core.EndOfStream p ->
            Core.EndOfStream
                { released = p.released }

        Core.ActiveStream p ->
            Core.EndOfStream
                { released = p.released ++ p.dependencies }


{-| Scans the Stream till the _scanner_ function returns `Nothing`. The scanner function receives new Stream data `a`, and the past result `b`. If the scanner returns `Nothing`, resulting `Stream` ends.
-}
scan : (a -> b -> Maybe b) -> b -> Stream a -> Stream b
scan f init stream =
    case stream of
        Core.ActiveStream param ->
            Core.ActiveStream
                { unwrapMsg =
                    \msg ->
                        let
                            ( data, next ) =
                                param.unwrapMsg msg

                            ( reversed, isComplete ) =
                                scanData [] f init data
                        in
                        if isComplete then
                            ( List.reverse reversed
                            , cancel next
                            )

                        else
                            ( List.reverse reversed
                            , scan f
                                (List.head reversed
                                    |> Maybe.withDefault init
                                )
                                next
                            )
                , dependencies = param.dependencies
                , released = param.released
                }

        Core.EndOfStream param ->
            Core.EndOfStream
                { released = param.released
                }


scanData : List b -> (a -> b -> Maybe b) -> b -> List a -> ( List b, Bool )
scanData acc f b ls =
    case ls of
        [] ->
            ( acc, False )

        x :: xs ->
            case f x b of
                Nothing ->
                    ( acc, True )

                Just b_ ->
                    scanData (b_ :: acc) f b_ xs


{-| Map Procedures to each Stream data asynchronously.

    import Tepa exposing (Promise)
    import Tepa.Time as Time

    sample : Promise m ()
    sample =
        Tepa.bind Time.tick <|
            \stream ->
                run
                    (\a ->
                        [ Debug.todo "procedures for each stream data `a`"
                        ]
                    )
                    stream

-}
run :
    (a -> List (Promise m ()))
    -> Stream a
    -> Promise m ()
run f stream =
    case stream of
        Core.EndOfStream param ->
            Core.releasePorts param.released

        Core.ActiveStream param ->
            Tepa.sequence
                [ Core.releasePorts param.released
                , Core.justAwaitPromise <|
                    \msg _ ->
                        let
                            ( ls, nextStream ) =
                                param.unwrapMsg msg
                        in
                        Tepa.syncAll
                            (List.map (Tepa.sequence << f) ls ++ [ run f nextStream ])
                ]



-- Convert to Promise


{-| Run Stream and wait for the first data.
-}
awaitFirst : Stream a -> Promise m a
awaitFirst stream =
    take 1 stream
        |> awaitAll
        |> Tepa.andThen
            (\ls ->
                case ls of
                    [] ->
                        Tepa.neverResolved

                    a :: _ ->
                        Tepa.succeed a
            )


{-| Similar to `awaitFirst`, but also specifies the timeout in milliseconds.
Returns `Nothing` if no data is produced by the timeout seconds after the Stream is run.
-}
awaitFirstWithTimeout : Int -> Stream a -> Promise m (Maybe a)
awaitFirstWithTimeout timeout stream =
    Tepa.bindAndThen (Core.tick timeout) <|
        \tickStream ->
            union
                [ map Just stream
                , map (\_ -> Nothing) tickStream
                ]
                |> take 1
                |> awaitAll
                |> Tepa.andThen
                    (\ls ->
                        case ls of
                            [] ->
                                Tepa.succeed Nothing

                            a :: _ ->
                                Tepa.succeed a
                    )


{-| Run Stream and wait for all the data the Stream produces.
Make sure that `awaitAll` is not resolved if the Stream continues to produce data indefinitely.
-}
awaitAll : Stream a -> Promise m (List a)
awaitAll =
    awaitAll_ []


awaitAll_ : List a -> Stream a -> Promise m (List a)
awaitAll_ reversed stream =
    case stream of
        Core.EndOfStream param ->
            Core.releasePorts param.released
                |> Tepa.andThen
                    (\() ->
                        Tepa.succeed <|
                            List.reverse reversed
                    )

        Core.ActiveStream param ->
            Core.releasePorts param.released
                |> Tepa.andThen
                    (\() ->
                        Core.justAwaitPromise <|
                            \msg _ ->
                                let
                                    ( data, nextStream ) =
                                        param.unwrapMsg msg
                                in
                                awaitAll_ (List.reverse data ++ reversed) nextStream
                    )


{-| Run Stream and continues to wait for all data as long as it meets the given condition.
When the Stream has finished producing all data while satisfying the condition, `awaitWhile` resolves to the all data.
-}
awaitWhile : (a -> Bool) -> Stream a -> Promise m (List a)
awaitWhile =
    awaitWhile_ []


awaitWhile_ : List a -> (a -> Bool) -> Stream a -> Promise m (List a)
awaitWhile_ reversed p stream =
    case stream of
        Core.EndOfStream param ->
            Core.releasePorts param.released
                |> Tepa.andThen
                    (\() ->
                        Tepa.succeed <|
                            List.reverse reversed
                    )

        Core.ActiveStream param ->
            Core.releasePorts param.released
                |> Tepa.andThen
                    (\() ->
                        Core.justAwaitPromise <|
                            \msg _ ->
                                let
                                    ( data, nextStream ) =
                                        param.unwrapMsg msg

                                    ( producedR, isComplete ) =
                                        awaitWhileHelper [] p data
                                in
                                awaitWhile_ (producedR ++ reversed) p <|
                                    if isComplete then
                                        Core.EndOfStream { released = param.dependencies }

                                    else
                                        nextStream
                    )


awaitWhileHelper :
    List a
    -> (a -> Bool)
    -> List a
    -> ( List a, Bool )
awaitWhileHelper acc p ls =
    case ls of
        [] ->
            ( acc, False )

        x :: xs ->
            if p x then
                ( acc, True )

            else
                awaitWhileHelper
                    (x :: acc)
                    p
                    xs


{-| Run Stream and continues to wait for all data as long as it does not meet the given condition.
When the Stream has finished producing all data while not satisfying the condition, `awaitUnless` resolves to the all data.
-}
awaitUnless : (a -> Bool) -> Stream a -> Promise m (List a)
awaitUnless p =
    awaitWhile (not << p)


{-| Similar to `awaitAll`, but also specifies the timeout in milliseconds.
Returns `Nothing` if no data is produced by the timeout seconds after the Stream is run.
-}
awaitAllWithTimeout : Int -> Stream a -> Promise m (List a)
awaitAllWithTimeout timeout stream =
    Tepa.bindAndThen (Core.tick timeout) <|
        \tickStream ->
            union
                [ map Just stream
                , map (\_ -> Nothing) tickStream
                ]
                |> awaitWhile ((/=) Nothing)
                |> Tepa.map (List.filterMap identity)


{-| Similar to `run`, but the handler stops monitoring the Stream when it receives `Err`.
After the first `Err` value is handled, `while` resolves to `()`.
-}
while :
    (Result err a -> List (Promise m ()))
    -> Stream (Result err a)
    -> Promise m ()
while f stream =
    case stream of
        Core.EndOfStream param ->
            Core.releasePorts param.released

        Core.ActiveStream param ->
            Core.releasePorts param.released
                |> Tepa.andThen
                    (\() ->
                        Core.justAwaitPromise <|
                            \msg _ ->
                                let
                                    ( data, nextStream ) =
                                        param.unwrapMsg msg

                                    ( reversed, isComplete ) =
                                        whileHelper [] f data
                                in
                                Tepa.syncAll
                                    (List.reverse reversed
                                        ++ [ while f <|
                                                if isComplete then
                                                    Core.EndOfStream { released = param.dependencies }

                                                else
                                                    nextStream
                                           ]
                                    )
                    )


whileHelper :
    List (Promise m ())
    -> (Result err a -> List (Promise m ()))
    -> List (Result err a)
    -> ( List (Promise m ()), Bool )
whileHelper acc f ls =
    case ls of
        [] ->
            ( acc, False )

        x :: xs ->
            case x of
                Err _ ->
                    ( Tepa.sequence (f x) :: acc, True )

                Ok _ ->
                    whileHelper
                        (Tepa.sequence (f x) :: acc)
                        f
                        xs



-- Helper functions


{-| Execute only one `Case` that is the first of all the cases to receive data on its stream.

Suppose there is a popup with a Save button and a Close button.
When the Save button is pressed, the client saves the input, leaves the popup open, and waits for the user to press the Save or Close button again.
When the Close button is pressed, the input is discarded and the popup is closed.

For such cases, `oneOf` and `continue` and `break` are useful.

    import Tepa exposing (Promise)
    import Tepa.Stream as Stream

    popupProcedure : Promise Memory ()
    popupProcedure =
        [ Stream.oneOf
            [ Stream.break
                (Tepa.viewEventStream
                    { type_ = "click"
                    , key = "popup_save"
                    }
                )
              <|
                \() ->
                    -- Just stop waiting for data.
                    []
            , Stream.continue
                (Tepa.viewEventStream
                    { type_ = "click"
                    , key = "popup_close"
                    }
                )
              <|
                \() ->
                    [ closePopup
                    ]
            , Stream.continue
                (Tepa.viewEventStream
                    { type_ = "click"
                    , key = "openPopupButton"
                    }
                )
              <|
                \() ->
                    [ openPopup
                    ]
            ]

        -- This clause is evaluated after the user clicks the "Save" button
        -- and all spawned procedures are completed.
        , Tepa.bind (Tepa.getValue "popup_input") <|
            \str ->
                [ saveInput str
                , Tepa.lazy <| \_ -> popupProcedure
                ]
        ]

-}
oneOf : List (Case m) -> Promise m ()
oneOf ls =
    Tepa.bindAll (List.map (\(Case p) -> p) ls) <|
        \streams ->
            [ union streams
                |> while
                    (\res ->
                        case res of
                            Err p ->
                                [ p ]

                            Ok p ->
                                [ p ]
                    )
            ]


{-| -}
type Case m
    = Case (Promise m (Stream (Result (Promise m ()) (Promise m ()))))


{-| Wait for other data, and execute the given procedure asynchronously.
-}
continue : Promise m (Stream a) -> (a -> List (Promise m ())) -> Case m
continue promise f =
    Tepa.map (map (f >> Tepa.sequence >> Ok)) promise
        |> Case


{-| Stop waiting for data immediately, and execute the given procedure.
-}
break : Promise m (Stream a) -> (a -> List (Promise m ())) -> Case m
break promise f =
    Tepa.map (map (f >> Tepa.sequence >> Err)) promise
        |> Case


{-| Create custom case.

    import Tepa
    import Tepa.Stream as Stream exposing (Case)

    breakAfterClickTwice : Case Memory
    breakAfterClickTwice =
        Stream.customCase
            (Tepa.viewEventStream
                { key = "twoStepButton"
                , type_ = "click"
                }
            )
        <|
            \() curr ->
                if curr.counter < 2 then
                    { break = False
                    , procedure =
                        [ Tepa.modify <|
                            \m ->
                                { m | counter = m.counter + 1 }
                        ]
                    }

                else
                    { break = True
                    , procedure = []
                    }

-}
customCase :
    Promise m (Stream a)
    ->
        (a
         -> m
         ->
            { break : Bool
            , procedure : List (Promise m ())
            }
        )
    -> Case m
customCase promise f =
    Tepa.succeed
        (\stream state ->
            map
                (\a ->
                    let
                        res =
                            f a state
                    in
                    if res.break then
                        Err <| Tepa.sequence res.procedure

                    else
                        Ok <| Tepa.sequence res.procedure
                )
                stream
        )
        |> Tepa.sync promise
        |> Tepa.sync Tepa.currentState
        |> Case
