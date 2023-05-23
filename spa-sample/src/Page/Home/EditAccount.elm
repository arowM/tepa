module Page.Home.EditAccount exposing
    ( request
    , EditAccount
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

{-| Module about edit account request.


# Request

@docs request
@docs EditAccount


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
type EditAccount
    = EditAccount EditAccount_


type alias EditAccount_ =
    { name : String
    }


{-| Request server for editing account.
-}
request : EditAccount -> Promise m e Response
request (EditAccount editAccount) =
    Http.post
        { url = endpointUrl
        , requestBody =
            Http.jsonBody <|
                JE.object
                    [ ( "name", JE.string editAccount.name )
                    ]
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
        , "edit-account"
        ]
        []



-- Response


{-| Successful response body.
-}
type alias GoodResponseBody =
    { name : String
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
        { url = "https://example.com/api/edit-account"
        , statusCode = 200
        , statusText = "OK"
        , headers = Dict.singleton "Set-Cookie" "sessionId=38afes7a8"
        }

    Http.GoodResponse successfulMeta
        """
        {
          "profile": {
            "name": "Sakura-chan"
          }
        }
        """
        |> response
    --> GoodResponse
    -->     { name = "Sakura-chan"
    -->     }

    -- "name" value cannot be null.
    Http.GoodResponse successfulMeta
        """
        {
          "profile": {
            "name": null
          }
        }
        """
        |> response
    --> FatalErrorResponse

    -- "name" field is required.
    Http.GoodResponse successfulMeta
        """
        {
          "profile": {
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
            "name": "Sakura-chan"
          }
        """
        |> response
    --> FatalErrorResponse

    -- Handles "LoginRequired" error code specially.
    Http.BadResponse
        ( { url = "https://example.com/api/edit-account"
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
        ( { url = "https://example.com/api/edit-account"
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
                        (JD.succeed GoodResponseBody
                            |> JDP.required "name" JD.string
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



-- Form decoding


{-| Represents current form status, which can be invalid.
-}
type alias Form =
    { name : String
    }


{-| Form validation errors.
-}
type FormError
    = IdRequired


{-| Format error to display UI.
-}
displayFormError : FormError -> String
displayFormError error =
    case error of
        IdRequired ->
            "ID is required."


{-| Decode form.
-}
fromForm : Form -> Result (List FormError) EditAccount
fromForm form =
    FD.run formDecoder form


{-|

    sample1 : Form
    sample1 =
        { name = ""
        }

    toFormErrors sample1
    --> [ IdRequired ]

    sample2 : Form
    sample2 =
        { name = "a"
        }

    toFormErrors sample2
    --> []

-}
toFormErrors : Form -> List FormError
toFormErrors form =
    case fromForm form of
        Ok _ ->
            []

        Err errs ->
            errs


formDecoder : FD.Decoder Form FormError EditAccount
formDecoder =
    FD.top EditAccount_
        |> FD.field (FD.lift .name formIdDecoder)
        |> FD.map EditAccount


formIdDecoder : FD.Decoder String FormError String
formIdDecoder =
    FD.identity
        |> FD.assert (FD.minLength IdRequired 1)
