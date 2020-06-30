module Tables.Stories exposing (stories, update)

import Element exposing (Element, fill)
import Msg
import Return as R exposing (Return)
import Tables.Model as Tables
import Tables.Msg as Tables
import UI.Internal.Basics exposing (maybeNotThen)
import UI.Internal.TypeNumbers as T
import UI.RenderConfig as RenderConfig exposing (RenderConfig)
import UI.Table as Table
    exposing
        ( cellFromText
        , columnFilterEditing
        , columnFilterEmpty
        , columnFiltering
        , columnMobileDetailsHide
        , columnMobileDetailsShow
        , columnWidthPixels
        , columnWidthPortion
        , columnsFilterEnd
        , columnsMobileDetailsEnd
        , columnsWidthEnd
        , header
        , headersEnd
        , rowEnd
        , table
        )
import UI.Text as Text exposing (Text)
import UI.TextField as TextField
import UIExplorer exposing (storiesOf)
import Utils exposing (iconsSvgSprite, story, storyWithModel)


update : Tables.Msg -> Tables.Model -> Return Tables.Msg Tables.Model
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )


stories cfg =
    storiesOf
        "Tables"
        []
