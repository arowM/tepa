module Page.Login.Login exposing
    ( request
    , Login
    , Response(..)
    , GoodResponseBody
    , Form
    , FormError(..)
    , displayFormError
    , fromForm
    , toFormErrors
    , response
    , method
    , endpointUrl
    )

{-| Module about login request.


# Request

@docs request
@docs Login


# Response

@docs Response
@docs GoodResponseBody


# Form decoding

If you are not familiar with the concept of _form decoding_, see [blog post](https://arow.info/posts/2019/form-decoding/).

@docs Form
@docs FormError
@docs displayFormError
@docs fromForm
@docs toFormErrors


# Endpoint specification

@docs response
@docs method
@docs endpointUrl

-}

import App.Session as Session exposing (Profile)
import Form.Decoder as FD
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import Tepa exposing (Promise)
import Tepa.Http as Http
import Url.Builder as Url



-- Request


{-| Validated request-ready data.
-}
type Login
    = Login Login_


type alias Login_ =
    { id : String
    , pass : String
    }


{-| Request server for login.
-}
request : Login -> Promise m e Response
request (Login login) =
    Http.request
        { method = method
        , headers = []
        , url = endpointUrl
        , requestBody =
            Http.jsonBody <|
                JE.object
                    [ ( "id", JE.string login.id )
                    , ( "pass", JE.string login.pass )
                    ]
        , timeout = Just 5000
        }
        |> Tepa.map response


{-| -}
method : String
method =
    "POST"


{-| -}
endpointUrl : String
endpointUrl =
    Url.absolute
        [ "api"
        , "login"
        ]
        []



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
    | IncorrectIdOrPasswordResponse
    | TemporaryErrorResponse
    | FatalErrorResponse


{-|

    import Dict
    import Tepa.Http as Http

    successfulMeta : Http.Metadata
    successfulMeta =
        { url = "https://example.com/api/login"
        , statusCode = 200
        , statusText = "OK"
        , headers = Dict.singleton "Set-Cookie" "auth_token=authenticated"
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

    -- Handles "IncorrectIdOrPassword" error code specially.
    Http.BadResponse
        ( { url = "https://example.com/api/login"
          , statusCode = 401
          , statusText = "Unauthorized"
          , headers = Dict.empty
          }
        )
        """
        {
          "code": "IncorrectIdOrPassword"
        }
        """
        |> response
    --> IncorrectIdOrPasswordResponse

    -- `FatalErrorResponse` on other error codes.
    Http.BadResponse
        ( { url = "https://example.com/api/login"
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
                                    if code == "IncorrectIdOrPassword" then
                                        JD.succeed IncorrectIdOrPasswordResponse

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



-- Form decoding


{-| Represents current form status, which can be invalid.
-}
type alias Form =
    { id : String
    , pass : String
    }


{-| Form validation errors.
-}
type FormError
    = IdRequired
    | PassRequired
    | IncorrectIdOrPassword


{-| Format error to display UI.
-}
displayFormError : FormError -> String
displayFormError error =
    case error of
        IdRequired ->
            "ID is required."

        PassRequired ->
            "Password is required."

        IncorrectIdOrPassword ->
            "Incorrect ID or password."


{-| Decode form.
-}
fromForm : Form -> Result (List FormError) Login
fromForm form =
    FD.run formDecoder form


{-|

    sample1 : Form
    sample1 =
        { id = ""
        , pass = "foo"
        }

    toFormErrors sample1
    --> [ IdRequired ]

    sample2 : Form
    sample2 =
        { id = "foo"
        , pass = ""
        }

    toFormErrors sample2
    --> [ PassRequired ]

    sample3 : Form
    sample3 =
        { id = "a"
        , pass = "2"
        }

    toFormErrors sample3
    --> []

-}
toFormErrors : Form -> List FormError
toFormErrors form =
    case fromForm form of
        Ok _ ->
            []

        Err errs ->
            errs


formDecoder : FD.Decoder Form FormError Login
formDecoder =
    FD.top Login_
        |> FD.field (FD.lift .id formIdDecoder)
        |> FD.field (FD.lift .pass formPassDecoder)
        |> FD.map Login


formIdDecoder : FD.Decoder String FormError String
formIdDecoder =
    FD.identity
        |> FD.assert (FD.minLength IdRequired 1)


formPassDecoder : FD.Decoder String FormError String
formPassDecoder =
    FD.identity
        |> FD.assert (FD.minLength PassRequired 1)
