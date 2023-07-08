module Tepa.Time exposing
    ( Posix, sleep, now, every, posixToMillis, millisToPosix
    , Zone, utc, here
    , toYear, toMonth, toDay, toWeekday, toHour, toMinute, toSecond, toMillis
    , Weekday(..), Month(..)
    )

{-| Library for working with time and time zones.

TEPA version of [Time module](https://package.elm-lang.org/packages/elm/time/latest/Time) and [`Process.sleep`](https://package.elm-lang.org/packages/elm/core/latest/Process#sleep).

You should not use the `Time` module with TEPA because the functions that the module exposes cannot recognize the emulated time lapse during Scenario testing.


# Time

@docs Posix, sleep, now, every, posixToMillis, millisToPosix


# Time Zones

@docs Zone, utc, here


# Human Times

@docs toYear, toMonth, toDay, toWeekday, toHour, toMinute, toSecond, toMillis


# Weeks and Months

@docs Weekday, Month

-}

import Internal.Core as Internal
import Tepa exposing (Promise)
import Time


{-| Alias for [`Time.Posix`](https://package.elm-lang.org/packages/elm/time/latest/Time#Posix).

A computer representation of time. It is the same all over Earth, so if we
have a phone call or meeting at a certain POSIX time, there is no ambiguity.

It is very hard for humans to _read_ a POSIX time though, so we use functions
like [`toHour`](#toHour) and [`toMinute`](#toMinute) to `view` them.

-}
type alias Posix =
    Time.Posix


{-| TEPA version of [`Process.sleep`](https://package.elm-lang.org/packages/elm/core/latest/Process#sleep).

Block progress on the current procedure for the given number of milliseconds. The JavaScript equivalent of this is `setTimeout` which lets you delay work until later.

-}
sleep : Int -> Promise m ()
sleep =
    Internal.sleep


{-| TEPA version of [`Time.now`](https://package.elm-lang.org/packages/elm/time/latest/Time#now).

Get the POSIX time at the moment when this Promise is evaluated.

-}
now : Promise m Posix
now =
    Internal.now


{-| TEPA version of [`Time.every`](https://package.elm-lang.org/packages/elm/time/latest/Time#every).

Get the current time periodically at the specified interval in milliseconds (like `1000` for a second or `60 * 1000` for a minute or `60 * 60 * 1000` for an hour).

-}
every :
    Int
    -> (Posix -> List (Promise m ()))
    -> Promise m ()
every =
    Internal.listenTimeEvery


{-| Same as [`Time.posixToMillis`](https://package.elm-lang.org/packages/elm/time/latest/Time#posixToMillis).

Turn a `Posix` time into the number of milliseconds since 1970 January 1
at 00:00:00 UTC. It was a Thursday.

-}
posixToMillis : Posix -> Int
posixToMillis =
    Time.posixToMillis


{-| Same as [`Time.millisToPosix`](https://package.elm-lang.org/packages/elm/time/latest/Time#millisToPosix).

Turn milliseconds into a `Posix` time.

-}
millisToPosix : Int -> Posix
millisToPosix =
    Time.millisToPosix



-- TIME ZONES


{-| Alias for [`Time.Zone`](https://package.elm-lang.org/packages/elm/time/latest/Time#Zone).

Information about a particular time zone.

Refer to the `Time.utc` documentation for more detailed notes.

See [`utc`](#utc), [`here`](#here), and [`Browser.Env`][env] to learn how to
obtain `Zone` values.

[env]: /packages/elm/browser/latest/Browser#Env

-}
type alias Zone =
    Time.Zone


{-| Alias for [`Time.utc`](https://package.elm-lang.org/packages/elm/time/latest/Time#utc).

The time zone for Coordinated Universal Time ([UTC])

Refer to the `Time.utc` documentation for more detailed notes.

-}
utc : Zone
utc =
    Time.utc


{-| TEPA version of [here](https://package.elm-lang.org/packages/elm/time/latest/Time#here).

    Produce a `Zone` based on the current UTC offset. You can use this to figure

out what day it is where you are:

    import Tepa exposing (Promise)
    import Tepa.Time as Time

    whatDayIsIt : Promise m Int
    whatDayIsIt =
        Tepa.suceed Time.toDay
            |> Tepa.sync Time.here
            |> Tepa.sync Time.now

Refer to the `Time.here` documentation for more detailed notes.

-}
here : Promise m Zone
here =
    Internal.here



-- DATES


{-| Alias for [`Time.toYear`](https://package.elm-lang.org/packages/elm/time/latest/Time#toYear).

What year is it?!

    import Tepa.Time exposing (toYear, utc, millisToPosix)

    toYear utc (millisToPosix 0) == 1970
    toYear nyc (millisToPosix 0) == 1969

    -- pretend `nyc` is the `Zone` for America/New_York.

-}
toYear : Zone -> Posix -> Int
toYear =
    Time.toYear


{-| Alias for [`Time.toMonth`](https://package.elm-lang.org/packages/elm/time/latest/Time#toMonth).

What month is it?!

    import Tepa.Time exposing (toMonth, utc, millisToPosix)

    toMonth utc (millisToPosix 0) == Jan
    toMonth nyc (millisToPosix 0) == Dec

    -- pretend `nyc` is the `Zone` for America/New_York.

-}
toMonth : Zone -> Posix -> Month
toMonth zone posix =
    case Time.toMonth zone posix of
        Time.Jan ->
            Jan

        Time.Feb ->
            Feb

        Time.Mar ->
            Mar

        Time.Apr ->
            Apr

        Time.May ->
            May

        Time.Jun ->
            Jun

        Time.Jul ->
            Jul

        Time.Aug ->
            Aug

        Time.Sep ->
            Sep

        Time.Oct ->
            Oct

        Time.Nov ->
            Nov

        Time.Dec ->
            Dec


{-| Alias for [`Time.toDay`](https://package.elm-lang.org/packages/elm/time/latest/Time#toDay).

What day is it?! (Days go from 1 to 31)

    import Tepa.Time exposing (toDay, utc, millisToPosix)

    toDay utc (millisToPosix 0) == 1
    toDay nyc (millisToPosix 0) == 31

    -- pretend `nyc` is the `Zone` for America/New_York.

-}
toDay : Zone -> Posix -> Int
toDay =
    Time.toDay


{-| TEPA version of [`Time.toWeekday`](https://package.elm-lang.org/packages/elm/time/latest/Time#toWeekday).

What day of the week is it?

    import Tepa.Time exposing (toWeekday, utc, millisToPosix)

    toWeekday utc (millisToPosix 0) == Thu
    toWeekday nyc (millisToPosix 0) == Wed

    -- pretend `nyc` is the `Zone` for America/New_York.

-}
toWeekday : Zone -> Posix -> Weekday
toWeekday zone posix =
    case Time.toWeekday zone posix of
        Time.Mon ->
            Mon

        Time.Tue ->
            Tue

        Time.Wed ->
            Wed

        Time.Thu ->
            Thu

        Time.Fri ->
            Fri

        Time.Sat ->
            Sat

        Time.Sun ->
            Sun


{-| Alias for [`Time.toHour`](https://package.elm-lang.org/packages/elm/time/latest/Time#toHour).

What hour is it? (From 0 to 23)

    import Tepa.Time exposing (toHour, utc, millisToPosix)

    toHour utc (millisToPosix 0) == 0  -- 12am
    toHour nyc (millisToPosix 0) == 19 -- 7pm

    -- pretend `nyc` is the `Zone` for America/New_York.

-}
toHour : Zone -> Posix -> Int
toHour =
    Time.toHour


{-| Alias for [`Time.toMinute`](https://package.elm-lang.org/packages/elm/time/latest/Time#toMinute).

What minute is it? (From 0 to 59)

    import Tepa.Time exposing (toMinute, utc, millisToPosix)

    toMinute utc (millisToPosix 0) == 0

This can be different in different time zones. Some time zones are offset
by 30 or 45 minutes!

-}
toMinute : Zone -> Posix -> Int
toMinute =
    Time.toMinute


{-| Alias for [`Time.toSecond`](https://package.elm-lang.org/packages/elm/time/latest/Time#toSecond).

What second is it?

    import Time exposing (toSecond, utc, millisToPosix)

    toSecond utc (millisToPosix    0) == 0
    toSecond utc (millisToPosix 1234) == 1
    toSecond utc (millisToPosix 5678) == 5

-}
toSecond : Zone -> Posix -> Int
toSecond =
    Time.toSecond


{-| Alias for [`Time.toMillis`](https://package.elm-lang.org/packages/elm/time/latest/Time#toMillis).

    import Tepa.Time exposing (toMillis, utc, millisToPosix)

    toMillis utc (millisToPosix    0) == 0
    toMillis utc (millisToPosix 1234) == 234
    toMillis utc (millisToPosix 5678) == 678

-}
toMillis : Zone -> Posix -> Int
toMillis =
    Time.toMillis



-- WEEKDAYS AND MONTHS


{-| TEPA version of [Time.Weekday](https://package.elm-lang.org/packages/elm/time/latest/Time#Weekday).

Represents a `Weekday` so that you can convert it to a `String` or `Int`
however you please. For example, if you need the Japanese representation, you
can say:

    toJapaneseWeekday : Weekday -> String
    toJapaneseWeekday weekday =
        case weekday of
            Mon ->
                "月"

            Tue ->
                "火"

            Wed ->
                "水"

            Thu ->
                "木"

            Fri ->
                "金"

            Sat ->
                "土"

            Sun ->
                "日"

-}
type Weekday
    = Mon
    | Tue
    | Wed
    | Thu
    | Fri
    | Sat
    | Sun


{-| TEPA version of [Time.Month](https://package.elm-lang.org/packages/elm/time/latest/Time#Month).

Represents a `Month` so that you can convert it to a `String` or `Int`
however you please. For example, if you need the Danish representation, you
can say:

    toDanishMonth : Month -> String
    toDanishMonth month =
        case month of
            Jan ->
                "januar"

            Feb ->
                "februar"

            Mar ->
                "marts"

            Apr ->
                "april"

            May ->
                "maj"

            Jun ->
                "juni"

            Jul ->
                "juli"

            Aug ->
                "august"

            Sep ->
                "september"

            Oct ->
                "oktober"

            Nov ->
                "november"

            Dec ->
                "december"

-}
type Month
    = Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec
