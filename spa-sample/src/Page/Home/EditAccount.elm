module Page.Home.EditAccount exposing
    ( request
    , EditAccount
    , Response(..)
    , GoodResponseBody
    , response
    , Form
    , FormError(..)
    , displayFormError
    , fromForm
    , toFormErrors
    )

{-| Module about edit account request.


# Request

@docs request
@docs EditAccount


# Response

@docs Response
@docs GoodResponseBody
@docs response


# Form decoding

If you are not familiar with the concept of _form decoding_, see [blog post](https://arow.info/posts/2019/form-decoding/).

@docs Form
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
type EditAccount
    = EditAccount EditAccount_


type alias EditAccount_ =
    { name : String
    }


{-| Request server for editing account.
-}
request : EditAccount -> (Tepa.HttpResult String -> msg) -> Cmd msg
request (EditAccount editAccount) toMsg =
    Http.post
        { url =
            Url.absolute
                [ "api"
                , "edit-profile-name"
                ]
                []
        , body =
            Http.jsonBody <|
                JE.object
                    [ ( "name", JE.string editAccount.name )
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
    | LoginRequired
    | OtherError


{-|

    import Dict
    import Http

    response
        { url = "https://example.com/api/edit-profile-name"
        , statusCode = 200
        , statusText = "OK"
        , headers = Dict.singleton "Set-Cookie" "sessionId=38afes7a8"
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
        { url = "https://example.com/api/edit-profile-name"
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
        { url = "https://example.com/api/edit-profile-name"
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
