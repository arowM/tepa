module Tepa.Navigation exposing
    ( pushPath
    , replacePath
    , back
    , forward
    , load, reload, reloadAndSkipCache
    )

{-| This module helps you manage the browser's URL yourself.

TEPA version of [Browser.Navigation](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation).

You should not use the `Browser.Navigation` module with TEPA because the page transitions caused by the module are not recognized by the TEPA Scenario test.

Refer to the `Browser.Navigation` documentation for more detailed notes.


# Navigate within Page

@docs pushPath
@docs replacePath
@docs back
@docs forward


# Navigate to other Pages

@docs load, reload, reloadAndSkipCache

-}

import AppUrl exposing (AppUrl)
import Browser.Navigation as Nav
import Internal.Core as Core
import Tepa exposing (NavKey, Promise)



-- Navigation Procedures


{-| TEPA version of [pushUrl](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#pushUrl).

Change the URL path, but do not trigger a page load. You can construct the URL path using [lydell/elm-app-url](https://package.elm-lang.org/packages/lydell/elm-app-url/latest/AppUrl).

This will add a new entry to the browser history.

Refer to the `Browser.Navigation.pushUrl` documentation for more detailed notes.

-}
pushPath :
    NavKey
    -> AppUrl
    -> Promise m ()
pushPath navKey path =
    Core.onGoingProcedure
        (\eff ->
            { eff
                | realCmds =
                    case navKey of
                        Core.SimKey ->
                            []

                        Core.RealKey key ->
                            [ Nav.pushUrl key <|
                                AppUrl.toString path
                            ]
                , logs =
                    [ Core.PushPath path
                    ]
            }
        )


{-| TEPA version of [replaceUrl](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#replaceUrl).

Change the URL path, but do not trigger a page load. You can construct the URL path using [lydell/elm-app-url](https://package.elm-lang.org/packages/lydell/elm-app-url/latest/AppUrl).

This _will not_ add a new entry to the browser history.

Refer to the `Browser.Navigation.replaceUrl` documentation for more detailed notes.

-}
replacePath :
    NavKey
    -> AppUrl
    -> Promise m ()
replacePath navKey path =
    Core.onGoingProcedure
        (\eff ->
            { eff
                | realCmds =
                    case navKey of
                        Core.SimKey ->
                            []

                        Core.RealKey key ->
                            [ Nav.replaceUrl key <|
                                AppUrl.toString path
                            ]
                , logs =
                    [ Core.ReplacePath path
                    ]
            }
        )


{-| TEPA version of [back](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#back).

Go back some number of pages. So `back 1` goes back one page, and `back 2` goes back two pages.

Refer to the `Browser.Navigation.back` documentation for more detailed notes.

-}
back : NavKey -> Int -> Promise m ()
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


{-| TEPA version of [forward](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#forward).

Go forward some number of pages. So `forward 1` goes forward one page, and `forward 2` goes forward two pages. If there are no more pages in the future, this will do nothing.

Refer to the `Browser.Navigation.back` documentation for more detailed notes.

-}
forward : NavKey -> Int -> Promise m ()
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


{-| TEPA version of [load](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#load).

Leave the current page and load the given URL. **This always results in a
page load**, even if the provided URL is the same as the current one.

    gotoElmWebsite : Promise m ()
    gotoElmWebsite =
        load "https://elm-lang.org"

Check out the [`lydell/elm-app-url`][app-url] package for help building URLs.

[app-url]: /packages/lydell/elm-app-url/latest

-}
load : String -> Promise m ()
load =
    Core.load


{-| TEPA version of [reload](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#reload).

Reload the current page. **This always results in a page load!**
This may grab resources from the browser cache, so use
[`reloadAndSkipCache`](#reloadAndSkipCache)
if you want to be sure that you are not loading any cached resources.

-}
reload : Promise m ()
reload =
    Core.reload False


{-| TEPA version of [reloadAndSkipCache](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#reloadAndSkipCache).

Reload the current page without using the browser cache. **This always
results in a page load!** It is more common to want [`reload`](#reload).

-}
reloadAndSkipCache : Promise m ()
reloadAndSkipCache =
    Core.reload True
