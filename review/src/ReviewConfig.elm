module ReviewConfig exposing (config)

import NoBooleanCase
import NoDebug.Log
import NoDebug.TodoOrToString
import NoMissingTypeAnnotation
import NoRedundantCons
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoRedundantConcat
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule exposing (Rule)



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
    , NoUnused.CustomTypeConstructors.rule []
    , NoRedundantConcat.rule
    ]
