port module ScenarioWorker exposing (Memory, main)

import Json.Decode as JD
import Json.Encode as JE exposing (Value)
import Scenario exposing (MarkupConfig, sections)
import Tepa exposing (Program, Promise)
import Tepa.Scenario as Scenario


main : Program Memory
main =
    Tepa.headless
        { init = {}
        , procedure = procedure
        }


type alias Memory =
    {}


port output_request : Tepa.PortRequest msg


port output_response : Tepa.PortResponse msg


type alias Flags =
    MarkupConfig


flagsDecoder : JD.Decoder Flags
flagsDecoder =
    JD.map MarkupConfig <|
        JD.field "dev" JD.bool


procedure : Value -> Promise Memory ()
procedure rawFlags =
    case JD.decodeValue flagsDecoder rawFlags of
        Err err ->
            output <|
                JE.object
                    [ ( "error", JE.string <| JD.errorToString err )
                    ]

        Ok config ->
            let
                result =
                    Scenario.toMarkdown
                        { title = "Sample scenario"
                        , sections = sections config
                        , config = Scenario.en_US
                        }
            in
            case result of
                Ok res ->
                    output <|
                        JE.object
                            [ ( "value", JE.string res )
                            ]

                Err err ->
                    output <|
                        JE.object
                            [ ( "error", JE.string <| Scenario.stringifyInvalidMarkup err ) ]


output : Value -> Promise Memory ()
output v =
    Tepa.portRequest
        { request = output_request
        , response = output_response
        , portName = "output"
        , requestBody = v
        }
        |> Tepa.void
