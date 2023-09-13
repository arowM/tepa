port module App.Dom exposing
    ( overwriteValue, OverwriteResponse(..)
    , response
    , portNames
    )

{-|

@docs overwriteValue, OverwriteResponse


# Endpoint specification

@docs response
@docs portNames

-}

import Json.Decode as JD
import Json.Encode as JE exposing (Value)
import Tepa exposing (PortRequest, PortResponse, Promise)


port app_dom_overwrite_value_request : PortRequest a


port app_dom_overwrite_value_response : PortResponse a


{-| -}
overwriteValue : String -> String -> Promise m OverwriteResponse
overwriteValue id value =
    Tepa.portRequest
        { request = app_dom_overwrite_value_request
        , response = app_dom_overwrite_value_response
        , portName = portNames.overwriteValue
        , requestBody =
            JE.object
                [ ( "id", JE.string id )
                , ( "value", JE.string value )
                ]
        }
        |> Tepa.map response


{-| -}
portNames :
    { overwriteValue : String
    }
portNames =
    { overwriteValue = "overwriteValue"
    }


{-|

    import Json.Decode as JD exposing (decodeString)

    decodeString JD.value """
      {
        "result": "Success"
      }
      """
      |> Result.map response
    --> Ok GoodOverwriteResponse

    decodeString JD.value """
      {
        "result": "ElementNotFound"
      }
      """
      |> Result.map response
    --> Ok OverwriteElementNotFound

-}
response : Value -> OverwriteResponse
response rawResponse =
    case JD.decodeValue responseDecoder rawResponse of
        Ok resp ->
            resp

        Err _ ->
            OverwriteFatalError


responseDecoder : JD.Decoder OverwriteResponse
responseDecoder =
    JD.field "result" JD.string
        |> JD.andThen
            (\res ->
                case res of
                    "Success" ->
                        JD.succeed GoodOverwriteResponse

                    "ElementNotFound" ->
                        JD.succeed OverwriteElementNotFound

                    _ ->
                        JD.fail "Invalid response"
            )


{-| -}
type OverwriteResponse
    = GoodOverwriteResponse
    | OverwriteElementNotFound
    | OverwriteFatalError
