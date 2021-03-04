module UI.Internal.Analytics exposing (Analytics(..), Property, TableAnalytics(..), encode)

import Json.Encode as Encode exposing (Value)


type Analytics
    = TableAnalytics TableAnalytics


type TableAnalytics
    = ApplyFilter Int
    | ClearFilter Int
    | SetSorting Int Bool
    | ClearSorting


type alias Property =
    ( String, Value )


encode : Analytics -> List Property
encode analytics =
    case analytics of
        TableAnalytics a ->
            encodeTable a


encodeAction : String -> Property
encodeAction name =
    ( "action", Encode.string name )


encodeColumn : Int -> Property
encodeColumn column =
    ( "column", Encode.int column )


encodeTable : TableAnalytics -> List Property
encodeTable analytics =
    case analytics of
        ApplyFilter column ->
            [ encodeAction "apply_filter"
            , encodeColumn column
            ]

        ClearFilter column ->
            [ encodeAction "clear_filter"
            , encodeColumn column
            ]

        SetSorting column reverse ->
            [ encodeAction "set_sorting"
            , encodeColumn column
            , ( "reverse", Encode.bool reverse )
            ]

        ClearSorting ->
            [ encodeAction "clear_sorting" ]
