module UI.Analytics exposing (Analytics(..), TableAnalytics(..), encode)

import Json.Encode as Encode exposing (Value)


type Analytics
    = TableAnalytics TableAnalytics


type TableAnalytics
    = ApplyFilter Int
    | ClearFilter Int
    | SetSorting Int Bool
    | ClearSorting


encode : Analytics -> Value
encode analytics =
    case analytics of
        TableAnalytics a ->
            encodeTable a


encodeAction : String -> ( String, Value )
encodeAction name =
    ( "action", Encode.string name )


encodeColumn : Int -> ( String, Value )
encodeColumn column =
    ( "column", Encode.int column )


encodeTable : TableAnalytics -> Value
encodeTable analytics =
    case analytics of
        ApplyFilter column ->
            Encode.object
                [ encodeAction "apply_filter"
                , encodeColumn column
                ]

        ClearFilter column ->
            Encode.object
                [ encodeAction "clear_filter"
                , encodeColumn column
                ]

        SetSorting column reverse ->
            Encode.object
                [ encodeAction "set_sorting"
                , encodeColumn column
                , ( "reverse", Encode.bool reverse )
                ]

        ClearSorting ->
            Encode.object [ encodeAction "clear_sorting" ]
