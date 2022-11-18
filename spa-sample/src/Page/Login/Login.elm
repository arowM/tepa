module Page.Login.Login exposing
    ( request
    , Login
    , Response
    , responseDecoder
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
@docs Response
@docs responseDecoder


# Form decoding

If you are not familiar with the concept of _form decoding_, see [blog post](https://arow.info/posts/2019/form-decoding/).

@docs Form
@docs initForm
@docs FormError
@docs displayFormError
@docs fromForm
@docs toFormErrors

-}

import App.Session exposing (Session)
import Form.Decoder as FD
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import Url.Builder as Url



-- Request


{-| Validated request-ready data.
-}
type Login
    = Login Login_


{-|

    import Json.Decode as JD

    sampleResponse : String
    sampleResponse = """
    {
      "profile": {
        "id": "Sakura-chan-ID"
      }
    }
    """

    JD.decodeString responseDecoder sampleResponse
    --> Ok { session = { id = "Sakura-chan-ID" } }

-}
responseDecoder : JD.Decoder Response
responseDecoder =
    JD.succeed Response
        |> JDP.required "profile" sessionDecoder


sessionDecoder : JD.Decoder Session
sessionDecoder =
    JD.succeed Session
        |> JDP.required "id" JD.string


type alias Login_ =
    { id : String
    , pass : String
    }


{-| Response type for `request`.
-}
type alias Response =
    { session : Session
    }


{-| Request server for login.
-}
request : Login -> (Result Http.Error Value -> msg) -> Cmd msg
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
        , expect =
            Http.expectJson toMsg JD.value
        }



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


{-| Format error to display UI.
-}
displayFormError : FormError -> String
displayFormError error =
    case error of
        IdRequired ->
            "ID is required."

        PassRequired ->
            "Password is required."


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
