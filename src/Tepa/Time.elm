module Tepa.Time exposing
    ( sleep
    , now
    , every
    )

{-| A module exposing emulatable, time-related functions.

@docs sleep
@docs now
@docs every

-}

import Internal.Core as Internal
import Tepa exposing (Promise, Void)
import Time exposing (Posix)


{-| TEPA version of [`Process.sleep`](https://package.elm-lang.org/packages/elm/core/latest/Process#sleep).

Block progress on the current procedure for the given number of milliseconds. The JavaScript equivalent of this is `setTimeout` which lets you delay work until later.

-}
sleep : Int -> Promise c m e Void
sleep =
    Internal.sleep


{-| TEPA version of [`Time.now`](https://package.elm-lang.org/packages/elm/time/latest/Time#now).

Get the POSIX time at the moment when this Promise is evaluated.

-}
now : Promise c m e Posix
now =
    Internal.now


{-| TEPA version of [`Time.every`](https://package.elm-lang.org/packages/elm/time/latest/Time#every).

Get the current time periodically at the specified interval in milliseconds (like `1000` for a second or `60 * 1000` for a minute or `60 * 60 * 1000` for an hour).

-}
every :
    Int
    -> (Posix -> List (Promise c m e Void))
    -> Promise c m e Void
every =
    Internal.listenTimeEvery
