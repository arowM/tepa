module Page.Login.Login exposing
    ( request
    , Login
    , Response(..)
    , GoodResponseBody
    , response
    , Form
    , initForm
    , FormError(..)
    , displayFormError
    , fromForm
    , toFormErrors
    )

{-| Module about login request.


# Request

@docs request
@docs Login


# Response

@docs Response
@docs GoodResponseBody
@docs response


# Form decoding

If you are not familiar with the concept of _form decoding_, see [blog post](https://arow.info/posts/2019/form-decoding/).

@docs Form
@docs initForm
@docs FormError
@docs displayFormError
@docs fromForm
@docs toFormErrors

-}

import App.Session exposing (Profile)
import Form.Decoder as FD
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import Tepa
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
request : Login -> (Tepa.HttpResult String -> msg) -> Cmd msg
request (Login login) toMsg =
    Http.post
        { url =
            Url.absolute
                [ "api"
                , "login"
                ]
                []
        , body =
            Http.jsonBody <|
                JE.object
                    [ ( "id", JE.string login.id )
                    , ( "pass", JE.string login.pass )
                    ]
        , expect = Tepa.expectStringResponse toMsg
        }



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
    | IncorrectIdOrPassword
    | OtherError


{-|

    import Dict
    import Http

    response
        { url = "https://example.com/api/login"
        , statusCode = 200
        , statusText = "OK"
        , headers = Dict.singleton "Set-Cookie" "auth_token=authenticated"
        }
        """
        {
          "profile": {
            "id": "Sakura-chan-ID",
            "name": "Sakura-chan"
          }
        }
        """
    --> GoodResponse
    -->     { profile =
    -->         { id = "Sakura-chan-ID"
    -->         , name = Just "Sakura-chan"
    -->         }
    -->     }

    response
        { url = "https://example.com/api/login"
        , statusCode = 200
        , statusText = "OK"
        , headers = Dict.singleton "Set-Cookie" "auth_token=authenticated"
        }
        """
        {
          "profile": {
            "ID": "Sakura-chan-ID",
            ,name": "Sakura-chan"
          }
        }
        """
    --> OtherError

    response
        { url = "https://example.com/api/login"
        , statusCode = 200
        , statusText = "OK"
        , headers = Dict.singleton "Set-Cookie" "auth_token=authenticated"
        }
        """
        {
          "profile": {
            "id": "Sakura-chan-ID",
            "name": "Sakura-chan"
          }
        """
    --> OtherError

    response
        { url = "https://example.com/api/login"
        , statusCode = 401
        , statusText = "Unauthorized"
        , headers = Dict.empty
        }
        """
        {
          "code": "IncorrectIdOrPassword"
        }
        """
    --> IncorrectIdOrPassword

    response
        { url = "https://example.com/api/login"
        , statusCode = 401
        , statusText = "Unauthorized"
        , headers = Dict.empty
        }
        """
        {
          "code": "OtherErrorCode"
        }
        """
    --> OtherError

    response
        { url = "https://example.com/api/login"
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
        case ( meta.statusCode, JD.decodeString badStatusDecoder str ) of
            ( 401, Ok body ) ->
                case body.code of
                    "IncorrectIdOrPassword" ->
                        IncorrectIdOrPassword

                    _ ->
                        OtherError

            _ ->
                OtherError


type alias BadStatusResponse =
    { code : String
    }


badStatusDecoder : JD.Decoder BadStatusResponse
badStatusDecoder =
    JD.map BadStatusResponse <|
        JD.field "code" JD.string


goodStatusDecoder : JD.Decoder GoodResponseBody
goodStatusDecoder =
    JD.succeed GoodResponseBody
        |> JDP.required "profile" profileDecoder


profileDecoder : JD.Decoder Profile
profileDecoder =
    JD.succeed Profile
        |> JDP.required "id" JD.string
        |> JDP.required "name" (JD.maybe JD.string)



-- Form decoding


{-| Represents current form status, which can be invalid.
-}
type alias Form =
    { id : String
    , pass : String
    }


{-| Initial value.
-}
initForm : Form
initForm =
    { id = ""
    , pass = ""
    }


{-| Form validation errors.
-}
type FormError
    = IdRequired
    | PassRequired
    | IncorrectIdOrPass


{-| Format error to display UI.
-}
displayFormError : FormError -> String
displayFormError error =
    case error of
        IdRequired ->
            "ID is required."

        PassRequired ->
            "Password is required."

        IncorrectIdOrPass ->
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
