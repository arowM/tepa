module App.FetchProfile exposing
    ( request
    , Response(..)
    , GoodResponseBody
    , response
    )

{-| Module about fetch profile request.


# Request

@docs request


# Response

@docs Response
@docs GoodResponseBody
@docs response

-}

import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import Tepa
import Url.Builder as Url



-- Request


{-| Request server for user profile.
-}
request : (Tepa.HttpResult String -> msg) -> Cmd msg
request toMsg =
    Http.post
        { url =
            Url.absolute
                [ "api"
                , "profile"
                ]
                []
        , body = Http.jsonBody <| JE.object []
        , expect = Tepa.expectStringResponse toMsg
        }



-- Response


{-| Successful response body.
-}
type alias GoodResponseBody =
    { id : String
    }


{-| Response type for `request`.
-}
type Response
    = GoodResponse GoodResponseBody
    | LoginRequired
    | OtherError


{-|

    import Dict
    import Http

    response
        { url = "https://example.com/api/profile"
        , statusCode = 200
        , statusText = "OK"
        , headers = Dict.singleton "Set-Cookie" "sessionId=38afes7a8"
        }
        """
        {
          "profile": {
            "id": "Sakura-chan-ID"
          }
        }
        """
    --> GoodResponse
    -->     { id = "Sakura-chan-ID"
    -->     }

    response
        { url = "https://example.com/api/profile"
        , statusCode = 200
        , statusText = "OK"
        , headers = Dict.singleton "Set-Cookie" "sessionId=38afes7a8"
        }
        """
        {
          "profile": {
          }
        }
        """
    --> OtherError

    response
        { url = "https://example.com/api/profile"
        , statusCode = 401
        , statusText = "Unauthorized"
        , headers = Dict.empty
        }
        """
        {
          "code": "AnyErrorCode"
        }
        """
    --> LoginRequired

    response
        { url = "https://example.com/api/profile"
        , statusCode = 404
        , statusText = "Not Found"
        , headers = Dict.empty
        }
        """
        {
          "code": "ValidErrorCode"
        }
        """
    --> OtherError

-}
response : Http.Metadata -> String -> Response
response meta str =
    if Tepa.isGoodStatus meta then
        case JD.decodeString goodStatusDecoder str of
            Ok body ->
                GoodResponse body

            Err _ ->
                OtherError

    else
        case meta.statusCode of
            401 ->
                LoginRequired

            _ ->
                OtherError


goodStatusDecoder : JD.Decoder GoodResponseBody
goodStatusDecoder =
    JD.succeed GoodResponseBody
        |> JDP.required "profile"
            (JD.field "id" JD.string)
