module App.Path exposing
    ( prefix
    , body
    , toAppUrl
    )

{-| Configuration and helper functions for the application URL path.

@docs prefix
@docs body
@docs toAppUrl

-}

import AppUrl exposing (AppUrl)
import Url


{-| URL path prefix.
-}
prefix : String
prefix =
    "tepa"


{-| Retrieve main part of your application URL.

    import Dict

    body
        { path = [ prefix, "my", "users" ]
        , queryParameters = Dict.empty
        , fragment = Nothing
        }
    --> Just [ "my", "users" ]

    body
        { path = [ "invalid-prefix", "my", "users" ]
        , queryParameters = Dict.empty
        , fragment = Nothing
        }
    --> Nothing

-}
body : AppUrl -> Maybe (List String)
body url =
    case url.path of
        [] ->
            Nothing

        prefix_ :: body_ ->
            if prefix_ == prefix then
                Just body_

            else
                Nothing


{-| Parse path string to get `AppUrl`.

    import Dict

    toAppUrl ("/foo/bar?p=a&p=b&p2=c#target")
    --> Just
    -->     { path = [ "foo", "bar" ]
    -->     , queryParameters = Dict.fromList [ ("p", [ "a", "b" ]), ("p2", [ "c" ]) ]
    -->     , fragment = Just "target"
    -->     }

    toAppUrl ("https://example.com/foo/bar?p=a&p=b&p2=c#target")
    --> Nothing

    toAppUrl ("foo/bar?p=a&p=b&p2=c#target")
    --> Nothing

-}
toAppUrl : String -> Maybe AppUrl
toAppUrl path =
    if String.startsWith "/" path then
        Url.fromString ("https://example.com" ++ path)
            |> Maybe.map AppUrl.fromUrl

    else
        Nothing
