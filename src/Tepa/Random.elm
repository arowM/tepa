module Tepa.Random exposing
    ( request, Spec
    , int, float, uniform, weighted
    , maxInt, minInt
    )

{-| This library helps you request pseudo-random values.

It is an implementation of [Permuted Congruential Generators][pcg]
by M. E. O'Neil. It is not cryptographically secure.

[extra]: /packages/elm-community/random-extra/latest
[pcg]: http://www.pcg-random.org/


# Request

@docs request, Spec


# Primitives

@docs int, float, uniform, weighted


# Constants

@docs maxInt, minInt

-}

import Internal.Core as Core
import Random
import Tepa exposing (Promise)


{-| A `Spec` is a specifcation for requesting random values. For example,
here is a specification for requesting numbers between 1 and 10 inclusive:

    import Tepa.Random as Random

    oneToTen : Random.Spec Int
    oneToTen =
        Random.int 1 10

Notice that we are not actually requesting any numbers yet!
We are just constructing the request body that describes what kind of values we want.
To actually get random values, you create a promise with the [`request`](#request) function:

    import Tepa exposing (Promise)
    import Tepa.Random as Random

    newNumber : Promise m Int
    newNumber =
        Random.request oneToTen

Each time you run this promise, it checks the `oneToTen` specification and produces
random integers between one and ten.

When testing scenarios, you specify the target request for your response with the `Spec`:

    import Tepa.Scenario as Scenario exposing (Scenario)

    respondToRandomInt : Scenario flags m e
    respondToRandomInt =
        Scenario.randomResponse
            mySession
            (Scenario.textContent "Respond `1` to the first unresolved `oneToTen` request.")
            { layer = myLayer
            , spec = oneToTen
            , value = 1
            }

    -- If there is no unresolved `oneToTen` request at the time, the test fails.

-}
type alias Spec a =
    Core.RandomSpec a


{-| Send a request for random values. Say you want to request
random points:

    import Tepa exposing (Promise)
    import Tepa.Random as Random

    pointX : Random.Spec Int
    pointX =
        Random.int -100 100

    pointY : Random.Spec Int
    pointY =
        Random.int -100 100

    newPoint : Promise m ( Int, Int )
    newPoint =
        Tepa.succeed Tuple.pair
            |> Tepa.sync (Random.request pointX)
            |> Tepa.sync (Random.request pointY)

Each time you run the `newPoint` promise, it will resolve to a new 2D point like
`(57, 18)` or `(-82, 6)`.

-}
request : Spec a -> Promise m a
request spec =
    case spec of
        Core.RandomSpecInt o ->
            Core.customRequest
                (\myRequestId msg _ ->
                    case msg of
                        Core.RandomResponseMsg param ->
                            if param.requestId == myRequestId then
                                o.unwrap param.response
                                    |> Maybe.map
                                        (\v ->
                                            ( v
                                            , [ Core.ResolveRandomRequest myRequestId ]
                                            )
                                        )

                            else
                                Nothing

                        _ ->
                            Nothing
                )
                (\myRequestId ->
                    Random.generate
                        (\n ->
                            Core.RandomResponseMsg
                                { requestId = myRequestId
                                , response = Core.RandomInt n
                                }
                        )
                        (Random.int o.min o.max)
                )
                (\myRequestId thisLayerId ->
                    Core.IssueRandomRequest myRequestId thisLayerId <|
                        Core.RequestRandomInt o.id
                )

        Core.RandomSpecFloat o ->
            Core.customRequest
                (\myRequestId msg _ ->
                    case msg of
                        Core.RandomResponseMsg param ->
                            if param.requestId == myRequestId then
                                o.unwrap param.response
                                    |> Maybe.map
                                        (\v ->
                                            ( v
                                            , [ Core.ResolveRandomRequest myRequestId ]
                                            )
                                        )

                            else
                                Nothing

                        _ ->
                            Nothing
                )
                (\myRequestId ->
                    Random.generate
                        (\a ->
                            Core.RandomResponseMsg
                                { requestId = myRequestId
                                , response = Core.RandomFloat a
                                }
                        )
                        (Random.float o.min o.max)
                )
                (\myRequestId thisLayerId ->
                    Core.IssueRandomRequest myRequestId thisLayerId <|
                        Core.RequestRandomFloat o.id
                )

        Core.RandomSpecEnum o ->
            let
                length =
                    List.length o.candidates
            in
            Core.customRequest
                (\myRequestId msg _ ->
                    case msg of
                        Core.RandomResponseMsg param ->
                            if param.requestId == myRequestId then
                                o.unwrap param.response
                                    |> Maybe.map
                                        (\v ->
                                            ( v
                                            , [ Core.ResolveRandomRequest myRequestId ]
                                            )
                                        )

                            else
                                Nothing

                        _ ->
                            Nothing
                )
                (\myRequestId ->
                    Random.generate
                        (\n ->
                            Core.RandomResponseMsg
                                { requestId = myRequestId
                                , response = Core.RandomEnum n
                                }
                        )
                        (Random.int 0 (length - 1))
                )
                (\myRequestId thisLayerId ->
                    Core.IssueRandomRequest myRequestId thisLayerId <|
                        Core.RequestRandomEnum o.id
                )


{-| Request 32-bit integers in a given range.

    import Tepa exposing (Promise)
    import Tepa.Random as Random

    singleDigit : Spec Int
    singleDigit =
        Random.int "singleDigit" 0 9

    closeToZero : Spec Int
    closeToZero =
        Random.int "closeToZero" -5 5

    anyInt : Spec Int
    anyInt =
        Random.int "anyInt" Random.minInt Random.maxInt

The string argument is the ID of the `Spec`, and is used to check the equivalence
of the two `Specs` in scenario testing. Be sure to specify unique IDs within the
same layer.

This promise _can_ produce values outside of the range [[`minInt`](#minInt),
[`maxInt`](#maxInt)] but sufficient randomness is not guaranteed.

_This is the TEPA version of [int](https://package.elm-lang.org/packages/elm/random/latest/Random#int)._

-}
int : String -> Int -> Int -> Spec Int
int id min max =
    Core.RandomSpecInt
        { min = min
        , max = max
        , id = id
        , unwrap =
            \v ->
                case v of
                    Core.RandomInt n ->
                        Just n

                    _ ->
                        Nothing
        , wrap =
            \n ->
                if n < min then
                    Err <|
                        "`"
                            ++ String.fromInt n
                            ++ "` is less than lower boundary `"
                            ++ String.fromInt min
                            ++ "`."

                else if max < n then
                    Err <|
                        "`"
                            ++ String.fromInt n
                            ++ "` is greater than upper boundary `"
                            ++ String.fromInt max
                            ++ "`."

                else
                    Ok <| Core.RandomInt n
        }


{-|

    import Tepa.Random as Random

    probability : Random.Spec Float
    probability =
        Random.float "probability" 0 1

The `probability` specification specifies values between zero and one with
a uniform distribution. Say it requests a value `p`. We can then check if
`p < 0.4` if we want something to happen 40% of the time.

The string argument is the ID of the `Spec`, and is used to check the equivalence
of the two `Specs` in scenario testing. Be sure to specify unique IDs within the
same layer.

_This is the TEPA version of [float](https://package.elm-lang.org/packages/elm/random/latest/Random#float)._

-}
float : String -> Float -> Float -> Spec Float
float id min max =
    Core.RandomSpecFloat
        { min = min
        , max = max
        , id = id
        , unwrap =
            \v ->
                case v of
                    Core.RandomFloat a ->
                        Just a

                    _ ->
                        Nothing
        , wrap =
            \a ->
                if a < min then
                    Err <|
                        "`"
                            ++ String.fromFloat a
                            ++ "` is less than lower boundary `"
                            ++ String.fromFloat min
                            ++ "`."

                else if max < a then
                    Err <|
                        "`"
                            ++ String.fromFloat a
                            ++ "` is greater than upper boundary `"
                            ++ String.fromFloat max
                            ++ "`."

                else
                    Ok <| Core.RandomFloat a
        }


{-| Request values with equal probability. Say we want a random suit for some
cards:

    import Tepa.Random as Spec

    type Suit
        = Diamond
        | Club
        | Heart
        | Spade

    suit : Random.Spec Suit
    suit =
        Random.uniform "suit" Diamond [ Club, Heart, Spade ]

That requests all `Suit` values with equal probability, 25% each.

The string argument is the ID of the `Spec`, and is used to check the equivalence
of the two `Specs` in scenario testing. Be sure to specify unique IDs within the
same layer.

**Note:** Why not have `uniform : String -> List a -> Spec a` as the API? It looks
a little prettier in code, but it leads to an awkward question. What do you do
with `uniform identifier []`? How can it request an `Int` or `Float`? The current API
guarantees that we always have _at least_ one value, so we never run into that
question!

-}
uniform : String -> a -> List a -> Spec a
uniform identifier value valueList =
    Core.RandomSpecEnum
        { id = identifier
        , candidates = ( 1, value ) :: List.map (\v -> ( 1, v )) valueList
        , unwrap =
            \v ->
                case v of
                    Core.RandomEnum n ->
                        List.drop n (value :: valueList)
                            |> List.head

                    _ ->
                        Nothing
        , wrap =
            \a ->
                (value :: valueList)
                    |> List.indexedMap Tuple.pair
                    |> List.filter (\( _, x ) -> x == a)
                    |> List.head
                    |> Maybe.map (Tuple.first >> Core.RandomEnum)
                    |> Result.fromMaybe
                        "The given value has no possibility of being a response."
        }


{-| Request values with a _weighted_ probability. Say we want to simulate a
[loaded die](https://en.wikipedia.org/wiki/Dice#Loaded_dice) that lands
on ⚄ and ⚅ more often than the other faces:

    import Tepa.Random as Random

    type Face
        = One
        | Two
        | Three
        | Four
        | Five
        | Six

    roll : Random.Sepc Face
    roll =
        Random.weighted "roll"
            ( 10, One )
            [ ( 10, Two )
            , ( 10, Three )
            , ( 10, Four )
            , ( 20, Five )
            , ( 40, Six )
            ]

So there is a 40% chance of getting `Six`, a 20% chance of getting `Five`, and
then a 10% chance for each of the remaining faces.

The string argument is the ID of the `Spec`, and is used to check the equivalence
of the two `Specs` in scenario testing. Be sure to specify unique IDs within the
same layer.

**Note:** I made the weights add up to 100, but that is not necessary. I always
add up your weights into a `total`, and from there, the probability of each case
is `weight / total`. Negative weights do not really make sense, so I just flip
them to be positive.

-}
weighted : String -> ( Float, a ) -> List ( Float, a ) -> Spec a
weighted identifier first others =
    Core.RandomSpecEnum
        { id = identifier
        , candidates = first :: others
        , unwrap =
            \v ->
                case v of
                    Core.RandomEnum n ->
                        List.drop n (first :: others)
                            |> List.head
                            |> Maybe.map Tuple.second

                    _ ->
                        Nothing
        , wrap =
            \a ->
                (first :: others)
                    |> List.indexedMap Tuple.pair
                    |> List.filter (\( _, ( p, x ) ) -> p > 0 && x == a)
                    |> List.head
                    |> Maybe.map (Tuple.first >> Core.RandomEnum)
                    |> Result.fromMaybe
                        "The given value has no possibility of being a response."
        }


{-| Alias for [`Random.maxInt`](https://package.elm-lang.org/packages/elm/random/latest/Random#maxInt).

The underlying algorithm works well in a specific range of integers.
It can request values outside of that range, but they are “not as random”.

The `maxInt` that works well is `2147483647`.

-}
maxInt : Int
maxInt =
    Random.maxInt


{-| Alias for [`Random.minInt`](https://package.elm-lang.org/packages/elm/random/latest/Random#minInt).

The underlying algorithm works well in a specific range of integers.
It can request values outside of that range, but they are “not as random”.

The `minInt` that works well is `-2147483648`.

-}
minInt : Int
minInt =
    Random.minInt
