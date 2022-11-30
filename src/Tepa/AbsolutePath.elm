module Tepa.AbsolutePath exposing
    ( AbsolutePath
    , absolutePath
    , fromUrl
    , toString
    )

{-|

@docs AbsolutePath
@docs absolutePath
@docs fromUrl
@docs toString

-}

import Internal.AbsolutePath as Internal
import Url exposing (Url)
import Url.Builder exposing (QueryParameter)


{-| Represent absolute URL path for your application.
-}
type alias AbsolutePath =
    Internal.AbsolutePath


{-| Constructor for `AbsolutePath`.

    import Url.Builder as Builder

    maybeNum : Maybe Int
    maybeNum = Just 4

    maybeString : Maybe String
    maybeString = Nothing

    absolutePath
        [ "foo"
        , "bar"
        ]
        [ Just <| Builder.string "key" "q1 q2"
        , Maybe.map (Builder.int "num") maybeNum
        , Maybe.map (Builder.string "filter") Nothing
        ]
        (Just "target")
        |> toString
    --> "/foo/bar?key=q1%20q2&num=4#target"

    absolutePath [] [] Nothing
        |> toString
    --> "/"

-}
absolutePath : List String -> List (Maybe QueryParameter) -> Maybe String -> AbsolutePath
absolutePath paths qs =
    Internal.absolutePath paths
        (List.filterMap identity qs)


{-| -}
fromUrl : Url -> AbsolutePath
fromUrl =
    Internal.fromUrl


{-| -}
toString : AbsolutePath -> String
toString =
    Internal.toString
