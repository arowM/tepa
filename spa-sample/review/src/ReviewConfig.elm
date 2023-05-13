module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Review.Rule as Rule exposing (Rule)
import NoAlways
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeExpose
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Simplify


config : List Rule
config =
    [ NoAlways.rule
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
        |> Rule.ignoreErrorsForFiles
            [ "src/Scenario.elm"
            ]
    , NoExposingEverything.rule
    , NoImportingEverything.rule []
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeExpose.rule
        |> Rule.ignoreErrorsForDirectories [ "src/Page" ]
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
        |> Rule.ignoreErrorsForDirectories
            [ "src/Widget"
            , "tests"
            ]
    , NoUnused.Modules.rule
        |> Rule.ignoreErrorsForDirectories [ "tests" ]
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , Simplify.defaults
        |> Simplify.ignoreCaseOfForTypes
            [ "Widget.Toast.Event"
            , "Page.Home.EditAccount.FormError"
            , "Scenario.Msg"
            ]
        |> Simplify.rule
        |> Rule.ignoreErrorsForFiles
            [ "tests/VerifyExample.elm"
            ]
    ]
        |> List.map (Rule.ignoreErrorsForDirectories [ "../src" ])
