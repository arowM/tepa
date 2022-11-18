module Tepa.Navigation exposing
    ( NavKey
    , Route
    , extractRoute
    , pushRoute
    , replaceRoute
    , back
    , forward
    )

{-| [Browser.Navigation](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation) alternative for TEPA.

This module helps you manage the browser’s URL yourself.


# Core

@docs NavKey
@docs Route
@docs extractRoute


# Navigation Procedures

@docs pushRoute
@docs replaceRoute
@docs back
@docs forward

-}

import Internal.Core as Core exposing (Promise, Void)
import Url exposing (Url)


{-| Alternative to [Browser.Navigation.Key](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#Key).

A navigation Key is needed to create navigation Procedures that change the URL.

-}
type alias NavKey =
    Core.NavKey


{-| Subset of [Url](https://package.elm-lang.org/packages/elm/url/latest/Url#Url) for your application URL.

Each field has the same meaning as `Url`.

-}
type alias Route =
    { path : String
    , query : Maybe String
    , fragment : Maybe String
    }


{-| -}
extractRoute : Url -> Route
extractRoute url =
    { path = url.path
    , query = url.query
    , fragment = url.fragment
    }



-- Navigation Procedures


{-| Alternative to [pushUrl](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#pushUrl).

Change the URL, but do not trigger a page load.

This will add a new entry to the browser history.

-}
pushRoute : NavKey -> Route -> Promise c m e Void
pushRoute key route =
    Core.pushAppCmd <|
        Core.PushRoute
            { key = key
            , route = route
            , replace = False
            }


{-| Alternative to [replaceUrl](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#replaceUrl).

Change the URL, but do not trigger a page load.

This _will not_ add a new entry to the browser history.

-}
replaceRoute : NavKey -> Route -> Promise c m e Void
replaceRoute key route =
    Core.pushAppCmd <|
        Core.PushRoute
            { key = key
            , route = route
            , replace = True
            }


{-| Alternative to [back](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#back).

Go back some number of pages. So `back 1` goes back one page, and `back 2` goes back two pages.

-}
back : NavKey -> Int -> Promise c m e Void
back key steps =
    Core.pushAppCmd <|
        Core.Back
            { key = key
            , steps = steps
            }


{-| Alternative to [forward](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Navigation#forward).

Go forward some number of pages. So `forward 1` goes forward one page, and `forward 2` goes forward two pages. If there are no more pages in the future, this will do nothing.

-}
forward : NavKey -> Int -> Promise c m e Void
forward key steps =
    Core.pushAppCmd <|
        Core.Back
            { key = key
            , steps = negate steps
            }
