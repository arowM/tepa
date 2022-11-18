module App.Route exposing
    ( Route(..)
    , fromUrl
    , toPath
    )

{-|


# Core

@docs Route
@docs fromUrl
@docs toPath

-}

import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), Parser, s)


pathPrefix : String
pathPrefix =
    "tepa"


{-| Requested routes.
-}
type Route
    = NotFound
    | Catalog
    | Home
    | Users


{-| -}
fromUrl : Url -> Route
fromUrl url =
    Parser.parse parser url
        |> Maybe.withDefault NotFound


parser : Parser (Route -> a) a
parser =
    s pathPrefix
        </> Parser.oneOf
                [ Parser.top
                    |> Parser.map Home
                , s "users"
                    |> Parser.map Users
                , s "catalog"
                    |> Parser.map Catalog
                ]


{-| -}
toPath : Route -> String
toPath route =
    case route of
        NotFound ->
            buildPath [ "not-found" ]

        Home ->
            buildPath []

        Users ->
            buildPath [ "users" ]

        Catalog ->
            buildPath [ "catalog" ]


buildPath : List String -> String
buildPath ps =
    Builder.absolute (pathPrefix :: ps) []
