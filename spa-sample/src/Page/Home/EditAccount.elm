module Page.Home.EditAccount exposing
    ( request
    , EditAccount
    , Response
    , responseDecoder
    , Form
    , initForm
    , FormError(..)
    , displayFormError
    , fromForm
    , toFormErrors
    )

{-| Module about edit account request.


# Request

@docs request
@docs EditAccount
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
type EditAccount
    = EditAccount EditAccount_


type alias EditAccount_ =
    { id : String
    }


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


{-| Response type for `request`.
-}
type alias Response =
    { session : Session
    }


{-| Request server for login.
-}
request : EditAccount -> (Result Http.Error Value -> msg) -> Cmd msg
request (EditAccount editAccount) toMsg =
    Http.post
        { url =
            Url.absolute
                [ "api"
                , "account"
                ]
                []
        , body =
            Http.jsonBody <|
                JE.object
                    [ ( "id", JE.string editAccount.id )
                    ]
        , expect =
            Http.expectJson toMsg JD.value
        }



-- Form decoding


{-| Represents current form status, which can be invalid.
-}
type alias Form =
    { id : String
    }


{-| Initial value.
-}
initForm : String -> Form
initForm text =
    { id = text
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
        { id = ""
        }

    toFormErrors sample1
    --> [ IdRequired ]

    sample2 : Form
    sample2 =
        { id = "a"
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
        |> FD.field (FD.lift .id formIdDecoder)
        |> FD.map EditAccount


formIdDecoder : FD.Decoder String FormError String
formIdDecoder =
    FD.identity
        |> FD.assert (FD.minLength IdRequired 1)
