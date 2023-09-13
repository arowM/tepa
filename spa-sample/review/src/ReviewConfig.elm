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
import NoUnoptimizedRecursion
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports exposing (annotatedBy)
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import NoDuplicatePorts
import NoUnsafePorts
import NoUnusedPorts
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
    , NoUnoptimizedRecursion.rule
        (NoUnoptimizedRecursion.optOutWithComment "IGNORE TCO")
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.defaults
        |> NoUnused.Exports.reportUnusedProductionExports
            { isProductionFile =
                \{ isInSourceDirectories } -> isInSourceDirectories
            , exceptionsAre = [ annotatedBy "@test-helper" ]
            }
        |> NoUnused.Exports.toRule
        |> Rule.ignoreErrorsForDirectories
            [ "src/Widget"
            , "tests"
            ]
    , NoUnused.Modules.rule
        |> Rule.ignoreErrorsForDirectories [ "tests" ]
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , NoDuplicatePorts.rule
    , NoUnsafePorts.rule NoUnsafePorts.any
    , NoUnusedPorts.rule
    , Simplify.defaults
        |> Simplify.rule
        |> Rule.ignoreErrorsForFiles
            [ "tests/VerifyExample.elm"
            ]
    ]
        |> List.map (Rule.ignoreErrorsForDirectories [ "../src" ])
