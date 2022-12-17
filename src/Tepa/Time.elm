module Tepa.Time exposing (sleep)

{-| A module exposing emulatable, time-related functions.

@docs sleep

-}

import Internal.Core as Internal
import Tepa exposing (Promise, Void)


{-| TEPA version of [`Process.sleep`](https://package.elm-lang.org/packages/elm/core/latest/Process#sleep).

Block progress on the current procedure for the given number of milliseconds. The JavaScript equivalent of this is `setTimeout` which lets you delay work until later.

-}
sleep : Float -> Promise c m e Void
sleep =
    Internal.sleep
