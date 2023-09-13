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

-}

import Internal.Core as Core exposing (PromiseState(..))
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
                }

        Core.EndOfStream ->
            Core.EndOfStream


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
                }

        Core.EndOfStream ->
            Core.EndOfStream


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
                            , Core.EndOfStream
                            )
                }

        Core.EndOfStream ->
            Core.EndOfStream


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
    List.foldl merge Core.EndOfStream


merge : Stream a -> Stream a -> Stream a
merge s1 s2 =
    case ( s1, s2 ) of
        ( Core.EndOfStream, Core.EndOfStream ) ->
            Core.EndOfStream

        ( Core.ActiveStream param1, Core.EndOfStream ) ->
            Core.ActiveStream param1

        ( Core.EndOfStream, Core.ActiveStream param2 ) ->
            Core.ActiveStream param2

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
                }


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
                            , Core.EndOfStream
                            )

                        else
                            ( List.reverse reversed
                            , scan f
                                (List.head reversed
                                    |> Maybe.withDefault init
                                )
                                next
                            )
                }

        Core.EndOfStream ->
            Core.EndOfStream


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
        Core.EndOfStream ->
            Tepa.none

        Core.ActiveStream param ->
            Core.Promise <|
                \context ->
                    let
                        nextPromise : Core.Msg -> m -> Promise m ()
                        nextPromise msg _ =
                            let
                                ( ls, nextStream ) =
                                    param.unwrapMsg msg
                            in
                            Tepa.syncAll
                                (List.map (Tepa.sequence << f) ls ++ [ run f nextStream ])
                    in
                    { newContext = context
                    , realCmds = []
                    , logs = []
                    , state = AwaitMsg nextPromise
                    }



-- Convert to Promise


{-| Run Stream and wait for the first data.
-}
awaitFirst : Stream a -> Promise m a
awaitFirst stream =
    let
        next : Stream a -> Core.Msg -> m -> Promise m a
        next stream_ msg _ =
            case stream_ of
                Core.EndOfStream ->
                    Core.Promise <|
                        \context ->
                            { newContext = context
                            , realCmds = []
                            , logs = []
                            , state = AwaitMsg <| next stream_
                            }

                Core.ActiveStream param ->
                    let
                        ( data, nextStream ) =
                            param.unwrapMsg msg
                    in
                    case data of
                        [] ->
                            Core.Promise <|
                                \context ->
                                    { newContext = context
                                    , realCmds = []
                                    , logs = []
                                    , state =
                                        AwaitMsg <|
                                            next nextStream
                                    }

                        a :: _ ->
                            Tepa.succeed a
    in
    Core.Promise <|
        \context ->
            { newContext = context
            , realCmds = []
            , logs = []
            , state = AwaitMsg <| next stream
            }


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
                |> awaitFirst


{-| Run Stream and wait for all the data the Stream produces.
Make sure that `awaitAll` is not resolved if the Stream continues to produce data indefinitely.
-}
awaitAll : Stream a -> Promise m (List a)
awaitAll stream =
    let
        next : Stream a -> List a -> Core.Msg -> m -> Promise m (List a)
        next stream_ reversed msg _ =
            case stream_ of
                Core.EndOfStream ->
                    List.reverse reversed
                        |> Tepa.succeed

                Core.ActiveStream param ->
                    let
                        ( data, nextStream ) =
                            param.unwrapMsg msg
                    in
                    Core.Promise <|
                        \context ->
                            { newContext = context
                            , realCmds = []
                            , logs = []
                            , state =
                                AwaitMsg <|
                                    next nextStream (List.reverse data ++ reversed)
                            }
    in
    Core.Promise <|
        \context ->
            { newContext = context
            , realCmds = []
            , logs = []
            , state = AwaitMsg <| next stream []
            }


{-| Run Stream and continues to wait for all data as long as it meets the given condition.
When the Stream has finished producing all data while satisfying the condition, `awaitWhile` resolves to the all data.
-}
awaitWhile : (a -> Bool) -> Stream a -> Promise m (List a)
awaitWhile p stream =
    let
        next : Stream a -> List a -> Core.Msg -> m -> Promise m (List a)
        next stream_ reversed msg _ =
            case stream_ of
                Core.EndOfStream ->
                    List.reverse reversed
                        |> Tepa.succeed

                Core.ActiveStream param ->
                    let
                        ( data, nextStream ) =
                            param.unwrapMsg msg

                        ( producedR, isComplete ) =
                            awaitWhileHelper [] p data
                    in
                    Core.Promise <|
                        \context ->
                            { newContext = context
                            , realCmds = []
                            , logs = []
                            , state =
                                if isComplete then
                                    Resolved
                                        ((producedR ++ reversed)
                                            |> List.reverse
                                        )

                                else
                                    AwaitMsg <|
                                        next nextStream (producedR ++ reversed)
                            }
    in
    Core.Promise <|
        \context ->
            { newContext = context
            , realCmds = []
            , logs = []
            , state = AwaitMsg <| next stream []
            }


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
        Core.EndOfStream ->
            Tepa.none

        Core.ActiveStream param ->
            Core.Promise <|
                \context ->
                    let
                        nextPromise : Core.Msg -> m -> Promise m ()
                        nextPromise msg _ =
                            let
                                ( data, nextStream ) =
                                    param.unwrapMsg msg

                                ( reversed, isComplete ) =
                                    whileHelper [] f data
                            in
                            Tepa.syncAll
                                (List.reverse reversed
                                    ++ (if isComplete then
                                            []

                                        else
                                            [ while f nextStream ]
                                       )
                                )
                    in
                    { newContext = context
                    , realCmds = []
                    , logs = []
                    , state = AwaitMsg nextPromise
                    }


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
