module App.Route exposing
    ( Route(..)
    , fromUrl
    , toAbsolutePath
    , LoginProps
    )

{-|


# Core

@docs Route
@docs fromUrl
@docs toAbsolutePath


# Props

@docs LoginProps

-}

import Tepa.AbsolutePath exposing (AbsolutePath, absolutePath)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((</>), (<?>), Parser, s)
import Url.Parser.Query as Query


pathPrefix : String
pathPrefix =
    "tepa"


{-| Requested routes.
-}
type Route
    = NotFound
    | Home
    | Users
    | Login LoginProps
    | Catalog


type alias LoginProps =
    { backUrl : Maybe Url
    }


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
                , s "login"
                    <?> Query.string "back"
                    |> Parser.map
                        (\murl ->
                            Login
                                { backUrl = Maybe.andThen Url.fromString murl
                                }
                        )
                , s "catalog"
                    |> Parser.map Catalog
                ]


{-| -}
toAbsolutePath : Route -> AbsolutePath
toAbsolutePath route =
    case route of
        NotFound ->
            absolutePath
                [ pathPrefix, "not-found" ]
                []
                Nothing

        Home ->
            absolutePath [ pathPrefix ] [] Nothing

        Users ->
            absolutePath [ pathPrefix, "users" ] [] Nothing

        Login login ->
            absolutePath
                [ pathPrefix, "login" ]
                [ Maybe.map (Builder.string "back")
                    (login.backUrl
                        |> Maybe.map Url.toString
                    )
                ]
                Nothing

        Catalog ->
            absolutePath [ pathPrefix, "catalog" ] [] Nothing
