module Tepa.Time exposing
    ( Posix, sleep, now, every, tick, posixToMillis, millisToPosix
    , Zone, utc, here
    , toYear, toMonth, toDay, toWeekday, toHour, toMinute, toSecond, toMillis
    , Weekday(..), Month(..)
    )

{-| Library for working with time and time zones.

You should not use the `Time` module with TEPA because the functions that the module exposes cannot recognize the emulated time lapse during Scenario testing.


# Time

@docs Posix, sleep, now, every, tick, posixToMillis, millisToPosix


# Time Zones

@docs Zone, utc, here


# Human Times

@docs toYear, toMonth, toDay, toWeekday, toHour, toMinute, toSecond, toMillis


# Weeks and Months

@docs Weekday, Month

-}

import Internal.Core as Internal
import Tepa exposing (Promise)
import Tepa.Stream exposing (Stream)
import Time


{-| A computer representation of time. It is the same all over Earth, so if we
have a phone call or meeting at a certain POSIX time, there is no ambiguity.

It is very hard for humans to _read_ a POSIX time though, so we use functions
like [`toHour`](#toHour) and [`toMinute`](#toMinute) to `view` them.

_This is an alias for [`Time.Posix`](https://package.elm-lang.org/packages/elm/time/latest/Time#Posix)._

-}
type alias Posix =
    Time.Posix


{-| Block progress on the current procedure for the given number of milliseconds. The JavaScript equivalent of this is `setTimeout` which lets you delay work until later.

_This is the TEPA version of [`Process.sleep`](https://package.elm-lang.org/packages/elm/core/latest/Process#sleep)._

-}
sleep : Int -> Promise m ()
sleep =
    Internal.sleep


{-| Get the POSIX time at the moment when this Promise is evaluated.

_This is the TEPA version of [`Time.now`](https://package.elm-lang.org/packages/elm/time/latest/Time#now)._

-}
now : Promise m Posix
now =
    Internal.now


{-| Get the current time periodically at the specified interval in milliseconds (like `1000` for a second or `60 * 1000` for a minute or `60 * 60 * 1000` for an hour).

_This is the TEPA version of [`Time.every`](https://package.elm-lang.org/packages/elm/time/latest/Time#every)._

-}
every :
    Int
    -> (Posix -> List (Promise m ()))
    -> Promise m ()
every =
    Internal.listenTimeEvery


{-| Similar to `every`, but returns `Stream`.

The Stream produces current time periodically at the specified interval in milliseconds (like `1000` for a second or `60 * 1000` for a minute or `60 * 60 * 1000` for an hour).

-}
tick : Int -> Promise m (Stream Posix)
tick =
    Internal.tick


{-| Turn a `Posix` time into the number of milliseconds since 1970 January 1
at 00:00:00 UTC. It was a Thursday.

_This is same as [`Time.posixToMillis`](https://package.elm-lang.org/packages/elm/time/latest/Time#posixToMillis)._

-}
posixToMillis : Posix -> Int
posixToMillis =
    Time.posixToMillis


{-| Turn milliseconds into a `Posix` time.

_This is same as [`Time.millisToPosix`](https://package.elm-lang.org/packages/elm/time/latest/Time#millisToPosix)._

-}
millisToPosix : Int -> Posix
millisToPosix =
    Time.millisToPosix



-- TIME ZONES


{-| Information about a particular time zone.

Refer to the `Time.utc` documentation for more detailed notes.

See [`utc`](#utc), [`here`](#here), and [`Browser.Env`][env] to learn how to
obtain `Zone` values.

[env]: /packages/elm/browser/latest/Browser#Env

_This is an alias for [`Time.Zone`](https://package.elm-lang.org/packages/elm/time/latest/Time#Zone)._

-}
type alias Zone =
    Time.Zone


{-| The time zone for Coordinated Universal Time ([UTC])

Refer to the `Time.utc` documentation for more detailed notes.

_This is an alias for [`Time.utc`](https://package.elm-lang.org/packages/elm/time/latest/Time#utc)._

-}
utc : Zone
utc =
    Time.utc


{-| Produce a `Zone` based on the current UTC offset. You can use this to figure

out what day it is where you are:

    import Tepa exposing (Promise)
    import Tepa.Time as Time

    whatDayIsIt : Promise m Int
    whatDayIsIt =
        Tepa.suceed Time.toDay
            |> Tepa.sync Time.here
            |> Tepa.sync Time.now

Refer to the `Time.here` documentation for more detailed notes.

_This is the TEPA version of [here](https://package.elm-lang.org/packages/elm/time/latest/Time#here)._

-}
here : Promise m Zone
here =
    Internal.here



-- DATES


{-| What year is it?!

    import Tepa.Time exposing (toYear, utc, millisToPosix)

    toYear utc (millisToPosix 0) == 1970
    toYear nyc (millisToPosix 0) == 1969

    -- pretend `nyc` is the `Zone` for America/New_York.

_This is an alias for [`Time.toYear`](https://package.elm-lang.org/packages/elm/time/latest/Time#toYear)._

-}
toYear : Zone -> Posix -> Int
toYear =
    Time.toYear


{-| What month is it?!

    import Tepa.Time exposing (toMonth, utc, millisToPosix)

    toMonth utc (millisToPosix 0) == Jan
    toMonth nyc (millisToPosix 0) == Dec

    -- pretend `nyc` is the `Zone` for America/New_York.

_This is an alias for [`Time.toMonth`](https://package.elm-lang.org/packages/elm/time/latest/Time#toMonth)._

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


{-| What day is it?! (Days go from 1 to 31)

    import Tepa.Time exposing (toDay, utc, millisToPosix)

    toDay utc (millisToPosix 0) == 1
    toDay nyc (millisToPosix 0) == 31

    -- pretend `nyc` is the `Zone` for America/New_York.

_This is an alias for [`Time.toDay`](https://package.elm-lang.org/packages/elm/time/latest/Time#toDay)._

-}
toDay : Zone -> Posix -> Int
toDay =
    Time.toDay


{-| What day of the week is it?

    import Tepa.Time exposing (toWeekday, utc, millisToPosix)

    toWeekday utc (millisToPosix 0) == Thu
    toWeekday nyc (millisToPosix 0) == Wed

    -- pretend `nyc` is the `Zone` for America/New_York.

_This is the TEPA version of [`Time.toWeekday`](https://package.elm-lang.org/packages/elm/time/latest/Time#toWeekday)._

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


{-| What hour is it? (From 0 to 23)

    import Tepa.Time exposing (toHour, utc, millisToPosix)

    toHour utc (millisToPosix 0) == 0  -- 12am
    toHour nyc (millisToPosix 0) == 19 -- 7pm

    -- pretend `nyc` is the `Zone` for America/New_York.

_This is an alias for [`Time.toHour`](https://package.elm-lang.org/packages/elm/time/latest/Time#toHour)._

-}
toHour : Zone -> Posix -> Int
toHour =
    Time.toHour


{-| What minute is it? (From 0 to 59)

    import Tepa.Time exposing (toMinute, utc, millisToPosix)

    toMinute utc (millisToPosix 0) == 0

This can be different in different time zones. Some time zones are offset
by 30 or 45 minutes!

_This is an alias for [`Time.toMinute`](https://package.elm-lang.org/packages/elm/time/latest/Time#toMinute)._

-}
toMinute : Zone -> Posix -> Int
toMinute =
    Time.toMinute


{-| What second is it?

    import Time exposing (toSecond, utc, millisToPosix)

    toSecond utc (millisToPosix    0) == 0
    toSecond utc (millisToPosix 1234) == 1
    toSecond utc (millisToPosix 5678) == 5

_This is an alias for [`Time.toSecond`](https://package.elm-lang.org/packages/elm/time/latest/Time#toSecond)._

-}
toSecond : Zone -> Posix -> Int
toSecond =
    Time.toSecond


{-|

    import Tepa.Time exposing (toMillis, utc, millisToPosix)

    toMillis utc (millisToPosix    0) == 0
    toMillis utc (millisToPosix 1234) == 234
    toMillis utc (millisToPosix 5678) == 678

_This is an alias for [`Time.toMillis`](https://package.elm-lang.org/packages/elm/time/latest/Time#toMillis)._

-}
toMillis : Zone -> Posix -> Int
toMillis =
    Time.toMillis



-- WEEKDAYS AND MONTHS


{-| Represents a `Weekday` so that you can convert it to a `String` or `Int`
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

_This is the TEPA version of [Time.Weekday](https://package.elm-lang.org/packages/elm/time/latest/Time#Weekday)._

-}
type Weekday
    = Mon
    | Tue
    | Wed
    | Thu
    | Fri
    | Sat
    | Sun


{-| Represents a `Month` so that you can convert it to a `String` or `Int`
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

_This is the TEPA version of [Time.Month](https://package.elm-lang.org/packages/elm/time/latest/Time#Month)._

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
