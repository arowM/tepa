module Tepa.Http exposing
    ( get
    , post
    , request
    , bytesRequest
    , Header
    , header
    , RequestBody
    , emptyBody
    , stringBody
    , jsonBody
    , fileBody
    , bytesBody
    , Response(..)
    , Metadata
    )

{-| Send HTTP requests.

TEPA version of [Http module](https://package.elm-lang.org/packages/elm/http/latest/Http).


# Requests

@docs get
@docs post
@docs request
@docs bytesRequest


# Header

@docs Header
@docs header


# Request Body

@docs RequestBody
@docs emptyBody
@docs stringBody
@docs jsonBody
@docs fileBody
@docs bytesBody


# Response

@docs Response
@docs Metadata

-}

import Bytes exposing (Bytes)
import Dict exposing (Dict)
import File exposing (File)
import Internal.Core as Core
import Json.Encode exposing (Value)
import Tepa exposing (Promise)



-- # Response


{-| Create a `GET` request.

    import Tepa exposing (Promise)
    import Tepa.Http as Http

    myProcedures : Promise m e ()
    myProcedures =
        [ Debug.todo "some procedures"
        , Tepa.bind
            (Http.get
                { url = "https://elm-lang.org/assets/public-opinion.txt"
                }
            )
          <|
            \result ->
                case result of
                    Err err ->
                        [ proceduresForFailure err
                        ]

                    Ok content ->
                        [ proceduersForReceivingResponse content
                        ]
        ]

You can use functions like [`expectString`](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode#decodeString) exposed by [`elm/json`](/packages/elm/json/latest) to interpret the response as JSON value.
In this example, we are expecting the response body to be a `String` containing
the full text of _Public Opinion_ by Walter Lippmann.

**Note:** Use [`elm/url`](/packages/elm/url/latest) to build reliable URLs.

-}
get :
    { url : String
    }
    -> Promise m e (Response String)
get param =
    request
        { method = "GET"
        , headers = []
        , url = param.url
        , requestBody = EmptyRequestBody
        , timeout = Nothing
        }


{-| Create a `POST` request. So imagine we want to send a POST request for
some JSON data. It might look like this:

    import Json.Decode as JD
    import Tepa exposing (Promise)
    import Tepa.Http as Http

    myProcedures : Promise m e ()
    myProcedures =
        [ Debug.todo "some procedures"
        , Tepa.bind
            (Http.post
                { url = "https://elm-lang.org/assets/public-opinion.txt"
                , requestBody = Http.EmptyRequestBody
                }
            )
          <|
            \result ->
                case result of
                    GoodResponse meta rawBody ->
                        case JD.decodeString (JD.list JD.string) content of
                            Err err ->
                                [ proceduresForInvalidResponseBody err
                                ]

                            Ok body ->
                                [ proceduresForJsonResponseBody body
                                ]

                    _ ->
                        defaultHttpHandler result
        ]

Notice that we are using [`decodeString`](/packages/elm/json/latest/Json-Decode#decodeString) to interpret the response
as JSON.

We did not put anything in the body of our request, but you can use constructors for the `RequestBody` if you need to send information to the server.

-}
post :
    { url : String
    , requestBody : RequestBody
    }
    -> Promise m e (Response String)
post param =
    request
        { method = "POST"
        , headers = []
        , url = param.url
        , requestBody = param.requestBody
        , timeout = Nothing
        }


{-| Create a custom request. For example, a PUT for files might look like this:

    import File exposing (File)
    import Tepa exposing (Promise)
    import Tepa.Http as Http

    type Msg
        = Uploaded (Result Http.Error ())

    upload :
        File
        ->
            Promise
                c
                m
                e
                (Result
                    Http.Error
                    ( Http.Metadata, String )
                )
    upload file =
        request
            { method = "PUT"
            , headers = []
            , url = "https://example.com/publish"
            , requestBody = fileBody file
            , timeout = Nothing
            }

It lets you set custom headers as needed. The timeout is the number of milliseconds you are willing to wait before giving up.

-}
request :
    { method : String
    , headers : List Header
    , url : String
    , requestBody : RequestBody
    , timeout : Maybe Int
    }
    -> Promise m e (Response String)
request param =
    Core.httpRequest
        { method = param.method
        , headers =
            List.map unwrapHeader param.headers
        , url = param.url
        , requestBody = toCoreRequestBody param.requestBody
        , timeout = param.timeout
        , tracker = Nothing
        }
        |> Core.mapPromise fromCoreResult


fromCoreResult : Result Core.HttpRequestError ( Metadata, a ) -> Response a
fromCoreResult error =
    case error of
        Err (Core.BadUrl str) ->
            BadUrl str

        Err Core.Timeout ->
            Timeout

        Err Core.NetworkError ->
            NetworkError

        Ok ( meta, body ) ->
            if isGoodStatus meta then
                GoodResponse meta body

            else
                BadResponse meta body


{-| -}
bytesRequest :
    { method : String
    , headers : List Header
    , url : String
    , requestBody : RequestBody
    , timeout : Maybe Int
    }
    -> Promise m e (Response Bytes)
bytesRequest param =
    Core.httpBytesRequest
        { method = param.method
        , headers =
            List.map unwrapHeader param.headers
        , url = param.url
        , requestBody = toCoreRequestBody param.requestBody
        , timeout = param.timeout
        , tracker = Nothing
        }
        |> Core.mapPromise fromCoreResult



-- Header


{-| An HTTP header for configuring requests.
-}
type Header
    = Header ( String, String )


unwrapHeader : Header -> ( String, String )
unwrapHeader (Header v) =
    v


{-| Create a `Header`.

    header "If-Modified-Since" "Sat 29 Oct 1994 19:43:31 GMT"

    header "Max-Forwards" "10"

    header "X-Requested-With" "XMLHttpRequest"

-}
header : String -> String -> Header
header name value =
    Header ( name, value )



-- Request Body


{-| Represents the body of a request.
-}
type RequestBody
    = EmptyRequestBody
    | StringRequestBody String String
    | JsonRequestBody Value
    | FileRequestBody File
    | BytesRequestBody String Bytes


toCoreRequestBody : RequestBody -> Core.HttpRequestBody
toCoreRequestBody body =
    case body of
        EmptyRequestBody ->
            Core.EmptyHttpRequestBody

        StringRequestBody mime str ->
            Core.StringHttpRequestBody mime str

        JsonRequestBody val ->
            Core.JsonHttpRequestBody val

        FileRequestBody file ->
            Core.FileHttpRequestBody file

        BytesRequestBody mime bytes ->
            Core.BytesHttpRequestBody mime bytes


{-| Create an empty body for your Request. This is useful for GET requests and POST requests where you are not sending any data.
-}
emptyBody : RequestBody
emptyBody =
    EmptyRequestBody


{-| Put some string in the body of your request. Defining `jsonRequest` looks like this:

    import Json.Encode as JE exposing (Value)

    jsonBody : Value -> Body
    jsonBody value =
        stringBody "application/json" (JE.encode 0 value)

The first argument is a [MIME type](https://developer.mozilla.org/docs/Web/HTTP/Basics_of_HTTP/MIME_types) of the body. Some servers are strict about this!

-}
stringBody : String -> String -> RequestBody
stringBody =
    StringRequestBody


{-| Put some JSON value in the body of your request. This will automatically add the `Content-Type: application/json` header.
-}
jsonBody : Value -> RequestBody
jsonBody =
    JsonRequestBody


{-| Use a file as the body of your request. When someone uploads an image into the browser with `elm/file` you can forward it to a server.

This will automatically set the `Content-Type` to the MIME type of the file.

**Note**: Use `track` to track upload progress.

-}
fileBody : File -> RequestBody
fileBody =
    FileRequestBody


{-| Put some `Bytes` in the body of your request. This allows you to use `elm/bytes` to have full control over the binary representation of the data you are sending. For example, you could create an `archive.zip` file and send it along like this:

    import Bytes exposing (Bytes)

    zipBody : Bytes -> RequestBody
    zipBody bytes =
      bytesBody "application/zip" bytes

    The first argument is a [MIME type](https://developer.mozilla.org/docs/Web/HTTP/Basics_of_HTTP/MIME_types) of the body. In other scenarios you may want to use MIME types like `image/png` or `image/jpeg` instead.

**Note**: Use `track` to track upload progress.

-}
bytesBody : String -> Bytes -> RequestBody
bytesBody =
    BytesRequestBody



-- Response
{- A `Response` can come back a couple different ways:

      - `BadUrl` means you did not provide a valid URL.
      - `Timeout` means it took too long to get a response.
      - `NetworkError` means the user turned off their wifi, went in a cave, etc.
      - `BadResponse` means you got a response back, but the status code indicates failure.
      - `GoodResponse` means you got a response back with a nice status code!

   The type of the `body` depends on whether you use
   [`request`](#request)
   or [`bytesRequest`](#bytesRequest).

-}


{-| -}
type Response body
    = BadUrl String
    | Timeout
    | NetworkError
    | BadResponse Metadata body
    | GoodResponse Metadata body


{-| Extra information about the response:

  - `url` of the server that actually responded (so you can detect redirects)
  - `statusCode` like `200` or `404`
  - `statusText` describing what the `statusCode` means a little
  - `headers` like `Content-Length` and `Expires`

**Note:** It is possible for a response to have the same header multiple times.
In that case, all the values end up in a single entry in the `headers`
dictionary. The values are separated by commas, following the rules outlined
[here](https://stackoverflow.com/questions/4371328/are-duplicate-http-response-headers-acceptable).

-}
type alias Metadata =
    { url : String
    , statusCode : Int
    , statusText : String
    , headers : Dict String String
    }


{-| Check if the response has a good status code.
-}
isGoodStatus : Metadata -> Bool
isGoodStatus meta =
    200 <= meta.statusCode && meta.statusCode < 300
