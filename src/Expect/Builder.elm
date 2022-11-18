module Expect.Builder exposing
    ( Builder
    , applyTo
    , runOn
    , equal, notEqual, all, oneOf
    , lessThan, atMost, greaterThan, atLeast
    , FloatingPointTolerance, within, notWithin
    , ok, err
    , equalLists, allOfListItems, oneOfListItem
    , equalDicts, equalSets
    , partial, fromJust, fromOk, fromErr
    , custom, pass, fail, onFail, extractOn, isPass
    )

{-| A library to build `Expectation`s flexibly.


## Quick Reference

  - [`equal`](#equal) `(arg2 == arg1)`
  - [`notEqual`](#notEqual) `(arg2 /= arg1)`
  - [`lessThan`](#lessThan) `(arg2 < arg1)`
  - [`atMost`](#atMost) `(arg2 <= arg1)`
  - [`greaterThan`](#greaterThan) `(arg2 > arg1)`
  - [`atLeast`](#atLeast) `(arg2 >= arg1)`
  - [Floating Point Comparisons](#floating-point-comparisons)


## Core

@docs Builder
@docs applyTo
@docs runOn


## Basic Expectation Builders

@docs equal, notEqual, all, oneOf


## Numeric Comparisons

@docs lessThan, atMost, greaterThan, atLeast


## Floating Point Comparisons

These functions allow you to compare `Float` values up to a specified rounding error, which may be relative, absolute,
or both. For an in-depth look, see our [Guide to Floating Point Comparison](#guide-to-floating-point-comparison).

@docs FloatingPointTolerance, within, notWithin


## Collections

@docs ok, err
@docs equalLists, allOfListItems, oneOfListItem
@docs equalDicts, equalSets


## Combinators

@docs partial, fromJust, fromOk, fromErr


## Customizing

These functions will let you build your own expectations.

@docs custom, pass, fail, onFail, extractOn, isPass


## Guide to Floating Point Comparison

See [Expect module](https://package.elm-lang.org/packages/elm-explorations/test/latest/Expect#guide-to-floating-point-comparison).

-}

import Dict exposing (Dict)
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Set exposing (Set)


{-| `Expectation` alternative.
-}
type Builder a
    = Builder (a -> Result Expectation ())


{-| Apply `Builder` to get `Expectation`.
-}
applyTo : Builder a -> a -> Expectation
applyTo (Builder f) a =
    case f a of
        Ok () ->
            Expect.pass

        Err e ->
            e


{-| Flipped version of `applyTo`.
-}
runOn : a -> Builder a -> Expectation
runOn a builder =
    applyTo builder a


{-| Build `Expect.all`.

Passes if each of the given functions passes when applied to the subject.
Passing an empty list is assumed to be a mistake, so `Expect.Builder.all []`
will always return a failed expectation no matter what else it is passed.

    Expect.Builder.all
        [ Expect.Builder.greaterThan -2
        , Expect.Builder.lessThan 5
        ]
        |> Expect.Builder.runOn (List.length [])
    -- Passes because (0 > -2) is True and (0 < 5) is also True

Failures resemble code written in pipeline style, so you can tell
which argument is which:
-- Fails because (0 < -10) is False
List.length []
|> Expect.Builder.applyTo
( Expect.Builder.all
[ Expect.Builder.greaterThan -2
, Expect.Builder.lessThan -10
, Expect.Builder.equal 0
]
)
{-
0
╷
│ Expect.lessThan
╵
-10
-}

-}
all : List (Builder a) -> Builder a
all ls =
    case ls of
        [] ->
            Builder <|
                \a ->
                    Err (Expect.all [] a)

        (Builder init) :: xs ->
            List.foldl
                (\(Builder f) acc a ->
                    case acc a of
                        Ok () ->
                            f a

                        Err e ->
                            Err e
                )
                init
                xs
                |> Builder


{-| Passes if one of the given functions passes when applied to the subject.
Passing an empty list is assumed to be a mistake, so `Expect.Builder.oneOf []`
will always return a failed expectation no matter what else it is passed.

    Expect.Builder.oneOf
        [ Expect.Builder.greaterThan 2
        , Expect.Builder.lessThan 5
        ]
        |> Expect.Builder.runOn (List.length [])
    -- Passes because (0 > 2) is False but (0 < 5) is also True

-}
oneOf : List (Builder a) -> Builder a
oneOf ls =
    case ls of
        [] ->
            Builder <|
                \_ ->
                    Err <| Expect.fail "Expect.Builder.oneOf was given an empty list. You must make at least one expectation to have a valid test!"

        (Builder init) :: xs ->
            Builder <|
                List.foldl
                    (\(Builder f) acc a ->
                        case acc a of
                            Ok () ->
                                Ok ()

                            Err _ ->
                                f a
                    )
                    init
                    xs
                    >> Result.mapError
                        (\_ -> Expect.fail "None of the Expect.Builder.oneOf elements passes.")


{-| Test for part of the subject.

    { foo = 3
    , bar = "bar"
    }
    |> Expect.Builder.applyTo
        ( Expect.Builder.partial .foo <|
            Expect.Builder.lessThan 4
        )
    -- Passes because the `foo` value is lessThan 4.

-}
partial : (a -> b) -> Builder b -> Builder a
partial f (Builder builder) =
    Builder <|
        \a ->
            builder (f a)


{-| Passes if the
[`Maybe`](https://package.elm-lang.org/packages/lang/core/latest/Maybe) is
an `Just a` and the `a` value passes the given `Builder`; otherwise fails.

    -- Passes
    List.head [ 1 ]
        |> Expect.Builder.applyTo
            ( Expect.Builder.fromJust <|
                Expect.Builder.lessThan 3
            )


    -- Fails
    List.head []
        |> Expect.Builder.applyTo
            ( Expect.Builder.fromJust <|
                Expect.Builder.lessThan 3
            )


    -- Fails
    List.head [ 4 ]
        |> Expect.Builder.applyTo
            ( Expect.Builder.fromJust <|
                Expect.Builder.lessThan 3
            )

    {-

    4
    ╷
    │ Expect.lessThan
    ╵
    3

    -}

-}
fromJust : Builder a -> Builder (Maybe a)
fromJust (Builder builder) =
    Builder <|
        \ma ->
            case ma of
                Nothing ->
                    Err <| Expect.fail "thought the subject is `Just`."

                Just a ->
                    builder a


{-| Passes if the
[`Result`](https://package.elm-lang.org/packages/lang/core/latest/Result) is
an `Ok a` and the `a` value passes the given `Builder`; otherwise fails.

    -- Passes
    String.toInt "1"
        |> Expect.Builder.applyTo
            ( Expect.Builder.fromOk <|
                Expect.Builder.lessThan 3
            )


    -- Fails
    String.toInt "not an int"
        |> Expect.Builder.applyTo
            ( Expect.Builder.fromOk <|
                Expect.Builder.lessThan 3
            )


    -- Fails
    List.head "4"
        |> Expect.Builder.applyTo
            ( Expect.Builder.fromOk <|
                Expect.Builder.lessThan 3
            )

    {-

    4
    ╷
    │ Expect.lessThan
    ╵
    3

    -}

-}
fromOk : Builder a -> Builder (Result e a)
fromOk (Builder builder) =
    Builder <|
        \res ->
            case res of
                Err _ ->
                    Err <| Expect.fail "thought the subject is `Ok`."

                Ok a ->
                    builder a


{-| Passes if the
[`Result`](https://package.elm-lang.org/packages/lang/core/latest/Result) is
an `Err e` and the `e` value passes the given `Builder`; otherwise fails.
-}
fromErr : Builder e -> Builder (Result e a)
fromErr (Builder builder) =
    Builder <|
        \res ->
            case res of
                Err e ->
                    builder e

                Ok _ ->
                    Err <| Expect.fail "thought the subject is `Err`."


{-| Build `Expect.equal`.

Passes if the arguments are equal.

    Expect.Builder.equal 0 (List.length [])

    -- Passes because (0 == 0) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because the expected value didn't split the space in "Betty Botter"
    String.split " " "Betty Botter bought some butter"
        |> Expect.Builder.applyTo
            ( Expect.Builder.equal [ "Betty Botter", "bought", "some", "butter" ]
            )

    {-

    [ "Betty", "Botter", "bought", "some", "butter" ]
    ╷
    │ Expect.equal
    ╵
    [ "Betty Botter", "bought", "some", "butter" ]

    -}

Do not equate `Float` values; use [`within`](#within) instead.

-}
equal : a -> Builder a
equal expected =
    Builder <|
        \a ->
            if a == expected then
                Ok ()

            else
                Err <| Expect.equal expected a


{-| Build `Expect.notEqual`.

Passes if the arguments are not equal.

    -- Passes because (11 /= 100) is True
    90 + 10
        |> Expect.Builder.applyTo
            ( Expect.Builder.notEqual 11
            )


    -- Fails because (100 /= 100) is False
    90 + 10
        |> Expect.Builder.applyTo
            ( Expect.Builder.notEqual 100
            )

    {-

    100
    ╷
    │ Expect.notEqual
    ╵
    100

    -}

-}
notEqual : a -> Builder a
notEqual unexpected =
    Builder <|
        \a ->
            if a /= unexpected then
                Ok ()

            else
                Err <| Expect.notEqual unexpected a


{-| Build `Expect.lessThan`.

Passes if the second argument is less than the first.

    Expect.Builder.lessThan 1
        |> Expect.Builder.runOn (List.length [])

    -- Passes because (0 < 1) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 < -1) is False
    List.length []
        |> Expect.Builder.applyTo
            ( Expect.Builder.lessThan -1
            )


    {-

    0
    ╷
    │ Expect.lessThan
    ╵
    -1

    -}

Do not equate `Float` values; use [`notWithin`](#notWithin) instead.

-}
lessThan : comparable -> Builder comparable
lessThan base =
    Builder <|
        \a ->
            if a < base then
                Ok ()

            else
                Err <| Expect.lessThan base a


{-| Build `Expect.atMost`.

Passes if the second argument is less than or equal to the first.

    Expect.Builder.atMost 1
        |> Expect.Builder.runOn (List.length [])

    -- Passes because (0 <= 1) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 <= -3) is False
    List.length []
        |> Expect.Builder.applyTo
            ( Expect.atMost -3
            )

    {-

    0
    ╷
    │ Expect.atMost
    ╵
    -3

    -}

-}
atMost : comparable -> Builder comparable
atMost base =
    Builder <|
        \a ->
            if a <= base then
                Ok ()

            else
                Err <| Expect.atMost base a


{-| Build `Expect.greaterThan`.

Passes if the second argument is greater than the first.

    Expect.Builder.greaterThan -2
        |> Expect.Builder.runOn (List.length [])

    -- Passes because (0 > -2) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 > 1) is False
    List.length []
        |> Expect.Builder.applyTo
            (Expect.greaterThan 1
            )

    {-

    0
    ╷
    │ Expect.greaterThan
    ╵
    1

    -}

-}
greaterThan : comparable -> Builder comparable
greaterThan base =
    Builder <|
        \a ->
            if a > base then
                Ok ()

            else
                Err <| Expect.greaterThan base a


{-| Build `Expect.atLeast`.

Passes if the second argument is greater than or equal to the first.

    Expect.Builder.atLeast -2
        |> Expect.Builder.runOn (List.length [])

    -- Passes because (0 >= -2) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 >= 3) is False
    List.length []
        |> Expect.Builder.applyTo
            ( Expect.atLeast 3
            )

    {-

    0
    ╷
    │ Expect.atLeast
    ╵
    3

    -}

-}
atLeast : comparable -> Builder comparable
atLeast base =
    Builder <|
        \a ->
            if a >= base then
                Ok ()

            else
                Err <| Expect.atLeast base a



-- Floating Point Comparisons


{-| Reexport `Expect.FloatingPointTolerance` for convenience.
-}
type alias FloatingPointTolerance =
    Expect.FloatingPointTolerance


{-| Build `Expect.within`.

Passes if the second and third arguments are equal within a tolerance
specified by the first argument. This is intended to avoid failing because of
minor inaccuracies introduced by floating point arithmetic.

    -- Fails because 0.1 + 0.2 == 0.30000000000000004 (0.1 is non-terminating in base 2)
    0.1 + 0.2 |> Expect.Builder.applyTo (Expect.Builder.equal 0.3)

    -- So instead write this test, which passes
    0.1
        + 0.2
        |> Expect.Builder.applyTo
            (Expect.Builder.within (Absolute 0.000000001) 0.3)

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because 3.14 is not close enough to pi
    3.14
        |> Expect.Builderwithin.applyTo
            (Expect.within (Absolute 0.0001) pi
            )

    {-

    3.14
    ╷
    │ Expect.within Absolute 0.0001
    ╵
    3.141592653589793

    -}

-}
within : FloatingPointTolerance -> Float -> Builder Float
within tolerance base =
    Builder <|
        \a ->
            if withinCompare tolerance base a then
                Ok ()

            else
                Err <| Expect.within tolerance base a


{-| Build `Expect.notWithin`.

Passes if (and only if) a call to `within` with the same arguments would have failed.

-}
notWithin : FloatingPointTolerance -> Float -> Builder Float
notWithin tolerance base =
    Builder <|
        \a ->
            if withinCompare tolerance base a then
                Err <| Expect.notWithin tolerance base a

            else
                Ok ()


{-| Build `Expect.ok`.

Passes if the
[`Result`](https://package.elm-lang.org/packages/lang/core/latest/Result) is
an `Ok` rather than `Err`. This is useful for tests where you expect not to see
an error, but you don't care what the actual result is.

_(Tip: If your function returns a `Maybe` instead, consider `Expect.notEqual Nothing`.)_

    -- Passes
    String.toInt "20"
        |> Expect.Builder.applyTo
            Expect.ok

Test failures will be printed with the unexpected `Err` value contrasting with
any `Ok`.

    -- Fails
    String.toInt "not an int"
        |> Expect.Builder.applyTo
            Expect.Builder.ok

    {-

    Err "not an int"
    ╷
    │ Expect.ok
    ╵
    Ok _

    -}

-}
ok : Builder (Result a b)
ok =
    Builder <|
        \result ->
            case result of
                Ok _ ->
                    Ok ()

                Err _ ->
                    Err <| Expect.ok result


{-| Build `Expect.err`.

Passes if the
[`Result`](http://package.elm-lang.org/packages/elm-lang/core/latest/Result) is
an `Err` rather than `Ok`. This is useful for tests where you expect to get an
error but you don't care what the actual error is.

_(Tip: If your function returns a `Maybe` instead, consider `Expect.equal Nothing`.)_

    -- Passes
    String.toInt "not an int"
        |> Expect.Builder.applyTo
            Expect.Builder.err

Test failures will be printed with the unexpected `Ok` value contrasting with
any `Err`.

    -- Fails
    String.toInt "20"
        |> Expect.Builder.applyTo
            Expect.Builder.err

    {-

    Ok 20
    ╷
    │ Expect.err
    ╵
    Err _

    -}

-}
err : Builder (Result a b)
err =
    Builder <|
        \result ->
            case result of
                Ok _ ->
                    Err <| Expect.err result

                Err _ ->
                    Ok ()


{-| Build `Expect.equalLists`.

Passes if the arguments are equal lists.

    -- Passes
    [ 1, 2, 3 ]
        |> Expect.Builder.applyTo
            (Expect.Builder.equalLists [ 1, 2, 3 ])

Failures resemble code written in pipeline style, so you can tell
which argument is which, and reports which index the lists first
differed at or which list was longer:

    -- Fails
    [ 1, 2, 4, 6 ]
        |> Expect.Builder.applyTo
            (Expect.equalLists [ 1, 2, 5 ])

    {-

    [1,2,4,6]
    first diff at index index 2: +`4`, -`5`
    ╷
    │ Expect.equalLists
    ╵
    first diff at index index 2: +`5`, -`4`
    [1,2,5]

    -}

-}
equalLists : List a -> Builder (List a)
equalLists expected =
    Builder <|
        \actual ->
            if expected == actual then
                Ok ()

            else
                Err <| Expect.equalLists expected actual


{-| Passes if given `Builder` passes when applied to one of the list elements.
Passing an empty list is assumed to be a mistake, so `Expect.Builder.oneOfListItem []`
will always return a failed expectation no matter what else it is passed.

    [ 3, 4, 5 ]
        |> Expect.Builder.applyTo
            (Expect.Builder.oneOfListItem
                (Expect.Builder.greaterThan 4)
            )
    -- Passes because (5 > 4) is True

-}
oneOfListItem : Builder a -> Builder (List a)
oneOfListItem builder =
    custom <|
        \ls ->
            if List.isEmpty ls then
                fail "Expect.Builder.oneOfListItem was given an empty list. You must make at least one expectation to have a valid test!"

            else if List.any (\a -> isPass (extractOn a builder)) ls then
                pass

            else
                fail "Expect.Builder.oneOfListItem passes for none of the given list elements"


{-| Passes if given `Builder` passes when applied to each of the list elements.
Passing an empty list is assumed to be a mistake, so `Expect.Builder.allListItems []`
will always return a failed expectation no matter what else it is passed.

    [ 3, 4 ]
        |> Expect.Builder.applyTo
            (Expect.Builder.allListItems
                (Expect.Builder.lessThan 6)
            )
    -- Passes because (3 < 6) is True and (4 < 6) is also True

Failures resemble code written in pipeline style, so you can tell
which argument is which:
-- Fails because (0 < -10) is False
[ 3, 4, 10 ]
|> Expect.Builder.applyTo
(Expect.Builder.allListItems
(Expect.Builder.lessThan 6)
)
{-
10
╷
│ Expect.lessThan
╵
6
-}

-}
allOfListItems : Builder a -> Builder (List a)
allOfListItems builder =
    custom <|
        \ls ->
            if List.isEmpty ls then
                fail "Expect.Builder.allOfListItems was given an empty list. You must make at least one expectation to have a valid test!"

            else
                List.foldl
                    (\a acc ->
                        if isPass pass then
                            extractOn a builder

                        else
                            acc
                    )
                    pass
                    ls


{-| Build `equalDicts`.

Passes if the arguments are equal dicts.

    -- Passes
    Dict.fromList [ ( 1, "one" ), ( 2, "two" ) ]
        |> Expect.Builder.applyTo
            (Expect.Builder.equalDicts (Dict.fromList [ ( 1, "one" ), ( 2, "two" ) ]))

Failures resemble code written in pipeline style, so you can tell
which argument is which, and reports which keys were missing from
or added to each dict:

    -- Fails
    (Dict.fromList [ ( 1, "one" ), ( 2, "too" ) ])
        |> Expect.Builder.applyTo
            (Expect.Builder.equalDicts (Dict.fromList [ ( 1, "one" ), ( 2, "two" ), ( 3, "three" ) ]))

    {-

    Dict.fromList [(1,"one"),(2,"too")]
    diff: -[ (2,"two"), (3,"three") ] +[ (2,"too") ]
    ╷
    │ Expect.equalDicts
    ╵
    diff: +[ (2,"two"), (3,"three") ] -[ (2,"too") ]
    Dict.fromList [(1,"one"),(2,"two"),(3,"three")]

    -}

-}
equalDicts : Dict comparable a -> Builder (Dict comparable a)
equalDicts expected =
    Builder <|
        \actual ->
            if Dict.toList expected == Dict.toList actual then
                Ok ()

            else
                Err <| Expect.equalDicts expected actual


{-| Build `equalSets`.

Passes if the arguments are equal sets.

    -- Passes
    Set.fromList [ 1, 2 ]
        |> Expect.Builder.applyTo
            (Expect.Builder.equalSets (Set.fromList [ 1, 2 ]))

Failures resemble code written in pipeline style, so you can tell
which argument is which, and reports which keys were missing from
or added to each set:

    -- Fails
    (Set.fromList [ 1, 2, 4, 6 ])
        |> Expect.Builder.applyTo
            (Expect.Builder.equalSets (Set.fromList [ 1, 2, 5 ]))

    {-

    Set.fromList [1,2,4,6]
    diff: -[ 5 ] +[ 4, 6 ]
    ╷
    │ Expect.equalSets
    ╵
    diff: +[ 5 ] -[ 4, 6 ]
    Set.fromList [1,2,5]

    -}

-}
equalSets : Set comparable -> Builder (Set comparable)
equalSets expected =
    Builder <|
        \actual ->
            if Set.toList expected == Set.toList actual then
                Ok ()

            else
                Err <| Expect.equalSets expected actual


{-| Custom Builder.

    import Json.Decode exposing (decodeString, int)
    import Test exposing (test)
    import Expect.Builder


    test "Json.Decode.int can decode the number 42." <|
        \_ ->
            decodeString int "42"
                |> Expect.Builder.applyTo
                    (Expect.Builder.custom <|
                        \v ->
                            case v of
                                Ok _ ->
                                    Expect.Builder.pass

                                Err err ->
                                    Expect.Builder.fail err
                    )

-}
custom : (a -> Builder ()) -> Builder a
custom f =
    Builder <|
        \a ->
            let
                (Builder builder) =
                    f a
            in
            builder ()


{-| Build `Expect.pass` for any subject.

Always passes.

-}
pass : Builder a
pass =
    Builder <| \_ -> Ok ()


{-| Build `Expect.fail` for any subject.

Fails with the given message.

-}
fail : String -> Builder a
fail str =
    Builder <|
        \_ ->
            Err <| Expect.fail str


{-| If the given expectation fails, replace its failure message with a custom one.

    "something"
        |> Expect.Builder.equal "something else"
        |> Expect.Builder.onFail "thought those two strings would be the same"

-}
onFail : String -> Builder a -> Builder a
onFail str (Builder builder) =
    Builder <|
        \a ->
            case builder a of
                Ok () ->
                    Ok ()

                Err expectation ->
                    expectation
                        |> Expect.onFail str
                        |> Err


{-| Provide an subject value to extract constant `Builder`.

e.g., you can build your original `oneOfListItem` function with `extractOn` and `isPass`.

    myOneOfListItem : Builder a -> Builder (List a)
    myOneOfListItem builder =
        custom <|
            \ls ->
                if List.isEmpty ls then
                    fail "Expect.Builder.oneOfListItem was given an empty list. You must make at least one expectation to have a valid test!"

                else if List.any (\a -> isPass (extractOn a builder)) ls then
                    pass

                else
                    fail "Expect.Builder.oneOfListItem passes for none of the given list elements"

-}
extractOn : a -> Builder a -> Builder ()
extractOn a (Builder builder) =
    Builder <|
        \() ->
            builder a


{-| Check if an extracted `Builder` passes.
-}
isPass : Builder () -> Bool
isPass (Builder builder) =
    builder () == Ok ()



{---- Private *floating point* helper functions from elm-explorations/test.


Copyright (c) 2016-2018 The elm-test contributors
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

* Neither the name of elm-test nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

----}


absolute : FloatingPointTolerance -> Float
absolute tolerance =
    case tolerance of
        Absolute val ->
            val

        AbsoluteOrRelative val _ ->
            val

        _ ->
            0


relative : FloatingPointTolerance -> Float
relative tolerance =
    case tolerance of
        Relative val ->
            val

        AbsoluteOrRelative _ val ->
            val

        _ ->
            0


withinCompare : FloatingPointTolerance -> Float -> Float -> Bool
withinCompare tolerance a b =
    let
        withinAbsoluteTolerance =
            a - absolute tolerance <= b && b <= a + absolute tolerance

        withinRelativeTolerance =
            (a - abs (a * relative tolerance) <= b && b <= a + abs (a * relative tolerance))
                || (b - abs (b * relative tolerance) <= a && a <= b + abs (b * relative tolerance))
    in
    (a == b) || withinAbsoluteTolerance || withinRelativeTolerance
