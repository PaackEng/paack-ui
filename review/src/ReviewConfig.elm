module ReviewConfig exposing (config)

import NoDebug.Log
import NoDebug.TodoOrToString
import NoBooleanCase
import Review.Rule exposing (Rule)
import NoRedundantConcat
import NoRedundantCons
import NoMissingTypeAnnotation
import NoUnused.Variables
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables

-- Add rule regarding redundant concat
-- NoUnused.CustomTypeConstructors
-- NoUnused.Exports
-- Add ExposeAll

-- Missing rules present in elm-analyse
-- MapNothingToNothing
-- FileLoadFailed (?)
-- UnnecessaryParens (Not needed since elm-format handles it)
-- UnnecessaryPortModule (Not needed since compiler reports it)
-- DuplicateImport (Not needed since elm-format handles it)

config : List Rule
config =
    [ NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
    , NoBooleanCase.rule
    , NoRedundantCons.rule
    , NoMissingTypeAnnotation.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    ]
