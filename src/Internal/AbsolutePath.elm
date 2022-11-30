module Internal.AbsolutePath exposing
    ( AbsolutePath(..)
    , AbsolutePath_
    , absolutePath
    , fromUrl
    , toString
    )

{-| Absolute path.

@docs AbsolutePath
@docs AbsolutePath_
@docs absolutePath
@docs fromUrl
@docs toString

-}

import Url exposing (Url)
import Url.Builder exposing (QueryParameter)


{-| Represent absolute path for your application.
-}
type AbsolutePath
    = AbsolutePath AbsolutePath_


{-| Internal type.
-}
type alias AbsolutePath_ =
    { path : String
    , query : Maybe String
    , fragment : Maybe String
    }


{-| Constructor.

    import Url.Builder as Builder

    absolutePath
        [ "foo"
        , "bar"
        ]
        [ Builder.string "key" "q1 q2"
        , Builder.int "num" 4
        ]
        (Just "target")
    --> AbsolutePath
    -->     { path = "/foo/bar"
    -->     , query = Just "key=q1%20q2&num=4"
    -->     , fragment = Just "target"
    -->     }

    absolutePath [] [] Nothing
    --> AbsolutePath
    -->     { path = "/"
    -->     , query = Nothing
    -->     , fragment = Nothing
    -->     }

-}
absolutePath : List String -> List QueryParameter -> Maybe String -> AbsolutePath
absolutePath path query fragment =
    AbsolutePath
        { path = "/" ++ String.join "/" path
        , query =
            case String.split "?" <| Url.Builder.toQuery query of
                [ "", q ] ->
                    Just q

                _ ->
                    Nothing
        , fragment = fragment
        }


{-| -}
fromUrl : Url -> AbsolutePath
fromUrl url =
    AbsolutePath
        { path = url.path
        , query = url.query
        , fragment = url.fragment
        }


{-| -}
toString : AbsolutePath -> String
toString (AbsolutePath r) =
    String.concat
        [ r.path
        , r.query
            |> Maybe.map (\str -> "?" ++ str)
            |> Maybe.withDefault ""
        , r.fragment
            |> Maybe.map (\str -> "#" ++ str)
            |> Maybe.withDefault ""
        ]
