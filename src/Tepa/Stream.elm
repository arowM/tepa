module Tepa.Stream exposing
    ( Stream
    , first, firstWithTimeout
    , all
    , map
    , run
    , filter
    , filterMap
    , take
    , union
    , control
    , Control
    , continue
    , break
    , firstOfAll
    , bind
    , race
    )

{-| Handle data stream.


# Stream

@docs Stream
@docs first, firstWithTimeout
@docs all
@docs map
@docs run
@docs filter
@docs filterMap
@docs take
@docs union


# Control

@docs control
@docs Control
@docs continue
@docs break


# Helper Functions

@docs firstOfAll
@docs bind
@docs race

-}

import Internal.Core as Core exposing (PromiseState(..))
import Tepa exposing (Promise)


{-| Data stream is a concept in which one or more data arrives one after another in real time.
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


{-| Evaluate mapped Procedures asynchronously.
-}
run : Stream (Promise m ()) -> Promise m ()
run stream =
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
                                ( promises, nextStream ) =
                                    param.unwrapMsg msg
                            in
                            Tepa.syncAll
                                (promises ++ [ run nextStream ])
                    in
                    { newContext = context
                    , realCmds = []
                    , logs = []
                    , state = AwaitMsg nextPromise
                    }



-- Scan


{-| Control Stream precisely.
-}
control : (a -> Control b) -> Stream a -> Stream b
control f stream =
    case stream of
        Core.EndOfStream ->
            Core.EndOfStream

        Core.ActiveStream param ->
            Core.ActiveStream
                { unwrapMsg =
                    \msg ->
                        let
                            ( data, nextStream ) =
                                param.unwrapMsg msg

                            accumed : Control b
                            accumed =
                                List.foldl
                                    (\a acc ->
                                        case ( acc, f a ) of
                                            ( Continue xs1, Continue xs2 ) ->
                                                Continue (xs1 ++ xs2)

                                            ( Continue xs1, Break xs2 ) ->
                                                Break (xs1 ++ xs2)

                                            ( Break xs1, _ ) ->
                                                Break xs1
                                    )
                                    (Continue [])
                                    data
                        in
                        case accumed of
                            Continue xs ->
                                ( xs, control f nextStream )

                            Break xs ->
                                ( xs, Core.EndOfStream )
                }


{-| -}
type Control a
    = Continue (List a)
    | Break (List a)


{-| Pass given values, and continue processing.
-}
continue : List a -> Control a
continue =
    Continue


{-| Pass given values, and complete processing.
-}
break : List a -> Control a
break =
    Break



-- Convert to Promise


{-| Takes the first data from the Stream as a Promise result.
-}
first : Stream a -> Promise m a
first stream =
    let
        awaitFirst : Stream a -> Core.Msg -> m -> Promise m a
        awaitFirst stream_ msg _ =
            case stream_ of
                Core.EndOfStream ->
                    Core.Promise <|
                        \context ->
                            { newContext = context
                            , realCmds = []
                            , logs = []
                            , state = AwaitMsg <| awaitFirst stream_
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
                                            awaitFirst nextStream
                                    }

                        a :: _ ->
                            Tepa.succeed a
    in
    Core.Promise <|
        \context ->
            { newContext = context
            , realCmds = []
            , logs = []
            , state = AwaitMsg <| awaitFirst stream
            }


{-| Similar to `first`, but also specifies the timeout in milliseconds. If no data produced by the Stream in the timeout seconds, it returns `Nothing`.
-}
firstWithTimeout : Int -> Stream a -> Promise m (Maybe a)
firstWithTimeout timeout stream =
    Tepa.bindAndThen (Core.tick timeout) <|
        \tickStream ->
            union
                [ map Just stream
                , map (\_ -> Nothing) tickStream
                ]
                |> first


{-| Wait for the Stream to close, and take all the data from the Stream as a Promise result.
-}
all : Stream a -> Promise m (List a)
all stream =
    let
        awaitAll : Stream a -> List a -> Core.Msg -> m -> Promise m (List a)
        awaitAll stream_ reversed msg _ =
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
                                    awaitAll nextStream (data ++ reversed)
                            }
    in
    Core.Promise <|
        \context ->
            { newContext = context
            , realCmds = []
            , logs = []
            , state = AwaitMsg <| awaitAll stream []
            }



-- Helper functions


{-| -}
firstOfAll :
    List (Promise m (Stream a))
    -> Promise m a
firstOfAll ps =
    List.foldl
        (\p pacc ->
            Tepa.succeed (\acc a -> a :: acc)
                |> Tepa.sync pacc
                |> Tepa.sync p
        )
        (Tepa.succeed [])
        ps
        |> Tepa.andThen
            (\streams ->
                List.reverse streams
                    |> union
                    |> first
            )


{-| -}
bind :
    Promise m (Stream a)
    -> (a -> List (Promise m ()))
    -> Promise m (Stream (Promise m ()))
bind prom f =
    Tepa.map
        (map (f >> Tepa.sequence))
        prom


{-| -}
race :
    List (Promise m (Stream (Promise m ())))
    -> Promise m ()
race ps =
    Tepa.bindAll ps <|
        \streams ->
            [ union streams
                |> take 1
                |> run
            ]
