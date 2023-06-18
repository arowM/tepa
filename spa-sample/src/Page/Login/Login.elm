module Page.Login.Login exposing
    ( request
    , Login
    , Response(..)
    , GoodResponseBody
    , keys
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

@docs keys
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
import Dict exposing (Dict)
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
request : Login -> Promise m Response
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


{-| Keys for form elements.
-}
keys :
    { loginFormId : String
    , loginFormPassword : String
    }
keys =
    { loginFormId = "loginFormId"
    , loginFormPassword = "loginFormPassword"
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
fromForm : Dict String String -> Result (List FormError) Login
fromForm =
    FD.run formDecoder


{-|

    import Dict

    Dict.fromList
        [ (keys.loginFormPassword, "foo")
        ]
        |> toFormErrors
    --> [ IdRequired ]

    Dict.fromList
        [ (keys.loginFormPassword, "foo")
        , (keys.loginFormId, "")
        ]
        |> toFormErrors
    --> [ IdRequired ]

    Dict.fromList
        [ (keys.loginFormId, "foo")
        , (keys.loginFormPassword, "")
        ]
        |> toFormErrors
    --> [ PassRequired ]

    Dict.fromList
        [ (keys.loginFormId, "a")
        , (keys.loginFormPassword, "2")
        ]
        |> toFormErrors
    --> []

-}
toFormErrors : Dict String String -> List FormError
toFormErrors form =
    case fromForm form of
        Ok _ ->
            []

        Err errs ->
            errs


formDecoder : FD.Decoder (Dict String String) FormError Login
formDecoder =
    FD.top Login_
        |> FD.field (FD.lift (Dict.get keys.loginFormId) formIdDecoder)
        |> FD.field (FD.lift (Dict.get keys.loginFormPassword) formPassDecoder)
        |> FD.map Login


formIdDecoder : FD.Decoder (Maybe String) FormError String
formIdDecoder =
    FD.identity
        |> FD.map (Maybe.withDefault "")
        |> FD.assert (FD.minLength IdRequired 1)


formPassDecoder : FD.Decoder (Maybe String) FormError String
formPassDecoder =
    FD.identity
        |> FD.map (Maybe.withDefault "")
        |> FD.assert (FD.minLength PassRequired 1)
