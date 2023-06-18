module App.FetchProfile exposing
    ( request
    , Response(..)
    , GoodResponseBody
    , response
    , method
    , endpointUrl
    )

{-| Module about fetch profile request.


# Request

@docs request


# Response

@docs Response
@docs GoodResponseBody


# Endpoint specification

@docs response
@docs method
@docs endpointUrl

-}

import App.Session as Session exposing (Profile)
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Tepa exposing (Promise)
import Tepa.Http as Http
import Url.Builder as Url



-- Request


{-| Request server for user profile.
-}
request : Promise m Response
request =
    Http.get
        { url = endpointUrl
        }
        |> Tepa.map response


endpointUrl : String
endpointUrl =
    Url.absolute
        [ "api"
        , "profile"
        ]
        []


method : String
method =
    "GET"



-- Response


{-| Successful response body.
-}
type alias GoodResponseBody =
    { profile : Profile
    }


{-| Response type for `request`.
-}
type Response
    = GoodResponse GoodResponseBody
    | LoginRequiredResponse
    | TemporaryErrorResponse
    | FatalErrorResponse


{-|

    import Dict
    import Tepa.Http as Http

    successfulMeta : Http.Metadata
    successfulMeta =
        { url = "https://example.com/api/profile"
        , statusCode = 200
        , statusText = "OK"
        , headers = Dict.singleton "Set-Cookie" "sessionId=38afes7a8"
        }

    Http.GoodResponse successfulMeta
        """
        {
          "profile": {
            "id": "Sakura-chan-ID",
            "name": "Sakura-chan"
          }
        }
        """
        |> response
    --> GoodResponse
    -->     { profile =
    -->         { id = "Sakura-chan-ID"
    -->         , name = Just "Sakura-chan"
    -->         }
    -->     }

    -- Accept null "name" value
    Http.GoodResponse successfulMeta
        """
        {
          "profile": {
            "id": "Sakura-chan-ID",
            "name": null
          }
        }
        """
        |> response
    --> GoodResponse
    -->     { profile =
    -->         { id = "Sakura-chan-ID"
    -->         , name = Nothing
    -->         }
    -->     }

    -- "name" field is required.
    Http.GoodResponse successfulMeta
        """
        {
          "profile": {
            "id": "Sakura-chan-ID"
          }
        }
        """
        |> response
    --> FatalErrorResponse

    -- `FatalErrorResponse` on Invalid JSON payload.
    Http.GoodResponse successfulMeta
        """
        {
          "profile": {
            "id": "Sakura-chan-ID",
            "name": "Sakura-chan"
          }
        """
        |> response
    --> FatalErrorResponse

    -- Handles "LoginRequired" error code specially.
    Http.BadResponse
        ( { url = "https://example.com/api/profile"
          , statusCode = 401
          , statusText = "Unauthorized"
          , headers = Dict.empty
          }
        )
        """
        {
          "code": "LoginRequired"
        }
        """
        |> response
    --> LoginRequiredResponse

    -- `FatalErrorResponse` on other error codes.
    Http.BadResponse
        ( { url = "https://example.com/api/profile"
          , statusCode = 401
          , statusText = "Unauthorized"
          , headers = Dict.empty
          }
        )
        """
        {
          "code": "OtherErrorCode"
        }
        """
        |> response
    --> FatalErrorResponse

    -- Bad URL error is not a temprary error, so it is `FatalErrorResponse`.
    Http.BadUrl "foobar"
        |> response
    --> FatalErrorResponse

    Http.NetworkError
        |> response
    --> TemporaryErrorResponse

    Http.Timeout
        |> response
    --> TemporaryErrorResponse

-}
response : Http.Response String -> Response
response rawResponse =
    case rawResponse of
        Http.GoodResponse _ rawBody ->
            let
                decoder : JD.Decoder Response
                decoder =
                    JD.field "profile"
                        (JD.succeed Session.Profile
                            |> JDP.required "id" JD.string
                            |> JDP.required "name" (JD.nullable JD.string)
                            |> JD.map GoodResponseBody
                            |> JD.map GoodResponse
                        )
            in
            JD.decodeString decoder rawBody
                |> Result.withDefault FatalErrorResponse

        Http.BadResponse _ rawBody ->
            let
                decoder =
                    JD.field "code"
                        (JD.string
                            |> JD.andThen
                                (\code ->
                                    if code == "LoginRequired" then
                                        JD.succeed LoginRequiredResponse

                                    else
                                        JD.fail "Unknown code"
                                )
                        )
            in
            JD.decodeString decoder rawBody
                |> Result.withDefault FatalErrorResponse

        Http.BadUrl _ ->
            FatalErrorResponse

        Http.NetworkError ->
            TemporaryErrorResponse

        Http.Timeout ->
            TemporaryErrorResponse
