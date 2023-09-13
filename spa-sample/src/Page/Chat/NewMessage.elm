port module Page.Chat.NewMessage exposing
    ( request
    , NewMessage
    , Response(..)
    , keys
    , FormError(..)
    , displayFormError
    , fromForm
    , toFormErrors
    , response
    , portName
    )

{-| Module about new chat message.


# Request

@docs request
@docs NewMessage


# Response

@docs Response


# Form decoding

If you are not familiar with the concept of _form decoding_, see [blog post](https://arow.info/posts/2019/form-decoding/).

@docs keys
@docs FormError
@docs displayFormError
@docs fromForm
@docs toFormErrors


# Endpoint specification

@docs response
@docs portName

-}

import Dict exposing (Dict)
import Form.Decoder as FD
import Json.Decode as JD
import Json.Encode as JE exposing (Value)
import Page.Chat.Message as Message exposing (ActiveUser, Message)
import Tepa exposing (PortRequest, PortResponse, Promise)


port page_chat_newMessage_request : PortRequest a


port page_chat_newMessage_response : PortResponse a



-- Request


{-| Validated request-ready data.
-}
type NewMessage
    = NewMessage NewMessage_


type alias NewMessage_ =
    { body : String
    }


{-| Request server to push new message.
-}
request : NewMessage -> Promise m Response
request (NewMessage newMessage) =
    Tepa.portRequest
        { request = page_chat_newMessage_request
        , response = page_chat_newMessage_response
        , portName = portName
        , requestBody =
            JE.object
                [ ( "message", JE.string newMessage.body )
                ]
        }
        |> Tepa.map response


{-| -}
portName : String
portName =
    "default"



-- Response


{-| Response type for `request`.
-}
type Response
    = GoodResponse Message
    | Disconnected
    | FatalError


{-|

    import Page.Chat.Message as Message
    import Json.Decode as JD exposing (decodeString)

    decodeString JD.value """
      {
        "result": "Success",
        "body": {
          "user": {
            "display-name": "Sakura-chan",
            "color": "#ff69b4"
          },
          "value": "Hi, Warunasubi-kun!"
        }
      }
      """
      |> Result.map response
    --> Ok <| GoodResponse <| Message.UserMessage
    -->   { user =
    -->     { displayName = "Sakura-chan"
    -->     , color = "#ff69b4"
    -->     }
    -->   , message = "Hi, Warunasubi-kun!"
    -->   }

-}
response : Value -> Response
response rawResponse =
    case JD.decodeValue responseDecoder rawResponse of
        Ok resp ->
            resp

        Err _ ->
            FatalError


responseDecoder : JD.Decoder Response
responseDecoder =
    JD.field "result" JD.string
        |> JD.andThen
            (\res ->
                case res of
                    "Success" ->
                        JD.field "body"
                            (JD.map2
                                (\user message ->
                                    Message.UserMessage
                                        { user = user
                                        , message = message
                                        }
                                )
                                (JD.field "user" userDecoder)
                                (JD.field "value" JD.string)
                            )
                            |> JD.map GoodResponse

                    "Disconnected" ->
                        JD.succeed Disconnected

                    "FatalError" ->
                        JD.succeed FatalError

                    _ ->
                        JD.fail "Invalid Response"
            )


userDecoder : JD.Decoder ActiveUser
userDecoder =
    JD.map2 ActiveUser
        (JD.field "display-name" JD.string)
        (JD.field "color" JD.string)



-- Form decoding


{-| Keys for form elements.
-}
keys :
    { body : String
    , submit : String
    }
keys =
    { body = "newMessageForm_body"
    , submit = "newMessageForm_submit"
    }


{-| Form validation errors.
-}
type FormError
    = BodyRequired


{-| Format error to display UI.
-}
displayFormError : FormError -> String
displayFormError error =
    case error of
        BodyRequired ->
            "Input your message."


{-| Decode form.
-}
fromForm : Dict String String -> Result (List FormError) NewMessage
fromForm form =
    FD.run formDecoder form


{-|

    import Dict

    Dict.fromList
        [ (keys.body, "")
        ]
        |> toFormErrors
    --> [ BodyRequired ]

    Dict.fromList
        [
        ]
        |> toFormErrors
    --> [ BodyRequired ]

    Dict.fromList
        [ (keys.body, "a")
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


formDecoder : FD.Decoder (Dict String String) FormError NewMessage
formDecoder =
    FD.top NewMessage_
        |> FD.field (FD.lift (Dict.get keys.body) formBodyDecoder)
        |> FD.map NewMessage


formBodyDecoder : FD.Decoder (Maybe String) FormError String
formBodyDecoder =
    FD.identity
        |> FD.map (Maybe.withDefault "")
        |> FD.assert (FD.minLength BodyRequired 1)
