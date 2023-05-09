module Tepa.Navigation exposing
    ( pushPath
    , replacePath
    , back
    , forward
    )

{-| [Browser.Navigation](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation) alternative for TEPA.

This module helps you manage the browser’s URL yourself.


# Navigation Procedures

@docs pushPath
@docs replacePath
@docs back
@docs forward

-}

import Browser.Navigation as Nav
import Internal.Core as Core
import Tepa exposing (NavKey, Promise, Void)
import Tepa.AbsolutePath as AbsolutePath exposing (AbsolutePath)



-- Navigation Procedures


{-| Alternative to [pushUrl](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#pushUrl).

Change the URL, but do not trigger a page load.

This will add a new entry to the browser history.

-}
pushPath : NavKey -> AbsolutePath -> Promise c m e Void
pushPath navKey path =
    Core.onGoingProcedure
        (\eff ->
            { eff
                | realCmds =
                    case navKey of
                        Core.SimKey ->
                            []

                        Core.RealKey key ->
                            [ Nav.pushUrl key <| AbsolutePath.toString path
                            ]
                , logs =
                    [ Core.PushPath path
                    ]
            }
        )


{-| Alternative to [replaceUrl](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#replaceUrl).

Change the URL, but do not trigger a page load.

This _will not_ add a new entry to the browser history.

-}
replacePath : NavKey -> AbsolutePath -> Promise c m e Void
replacePath navKey path =
    Core.onGoingProcedure
        (\eff ->
            { eff
                | realCmds =
                    case navKey of
                        Core.SimKey ->
                            []

                        Core.RealKey key ->
                            [ Nav.replaceUrl key <| AbsolutePath.toString path
                            ]
                , logs =
                    [ Core.ReplacePath path
                    ]
            }
        )


{-| Alternative to [back](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#back).

Go back some number of pages. So `back 1` goes back one page, and `back 2` goes back two pages.

-}
back : NavKey -> Int -> Promise c m e Void
back navKey steps =
    Core.onGoingProcedure
        (\eff ->
            { eff
                | realCmds =
                    case navKey of
                        Core.SimKey ->
                            []

                        Core.RealKey key ->
                            [ Nav.back key steps
                            ]
                , logs =
                    [ Core.Back steps
                    ]
            }
        )


{-| Alternative to [forward](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#forward).

Go forward some number of pages. So `forward 1` goes forward one page, and `forward 2` goes forward two pages. If there are no more pages in the future, this will do nothing.

-}
forward : NavKey -> Int -> Promise c m e Void
forward navKey steps =
    Core.onGoingProcedure
        (\eff ->
            { eff
                | realCmds =
                    case navKey of
                        Core.SimKey ->
                            []

                        Core.RealKey key ->
                            [ Nav.forward key steps
                            ]
                , logs =
                    [ Core.Forward steps
                    ]
            }
        )
