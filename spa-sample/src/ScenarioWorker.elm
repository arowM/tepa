port module ScenarioWorker exposing (main)

import Json.Encode as JE exposing (Value)
import Scenario exposing (MarkupConfig, sections)
import Tepa.Scenario as Scenario


main : Program MarkupConfig () msg
main =
    Platform.worker
        { init =
            \config ->
                ( ()
                , let
                    result =
                        Scenario.toMarkdown
                            { title = "Sample scenario"
                            , sections = sections config
                            , config = Scenario.en_US
                            }
                  in
                  output <|
                    JE.string <|
                        case result of
                            Ok res ->
                                res

                            Err _ ->
                                "Error"
                )
        , update =
            \_ _ ->
                ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


port output : Value -> Cmd msg
