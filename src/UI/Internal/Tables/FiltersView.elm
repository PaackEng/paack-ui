module UI.Internal.Tables.FiltersView exposing (Config, header, headerSelectToggle)

-- WARNING: Don't use any other Size.* beyond "contextSize"

import Element exposing (Element, px)
import Element.Background as Background
import UI.Icon as Icon
import UI.Internal.Colors as Colors
import UI.Internal.Filter.Model exposing (Filter)
import UI.Internal.Filter.Sorter exposing (SortingDirection(..))
import UI.Internal.Filter.View as FilterV2
import UI.Internal.Primitives as Primitives
import UI.Internal.RenderConfig exposing (localeTerms)
import UI.Internal.Size as Size exposing (Size)
import UI.Internal.Tables.Filters as Filters
import UI.Internal.Tables.Sorters as Sorters
import UI.RenderConfig exposing (RenderConfig)
import UI.Utils.ARIA as ARIA
import UI.Utils.Element as Element


type alias Config msg =
    { openMsg : msg
    , discardMsg : msg
    , fromFiltersMsg : Filters.Msg -> msg
    , fromSortersMsg : Sorters.Msg -> msg
    , index : Int
    , label : String
    , isOpen : Bool
    }


header :
    RenderConfig
    -> Filter msg item
    -> Sorters.ColumnStatus item
    -> Config msg
    -> Element msg
header renderConfig filter sorting config =
    let
        sortMsg =
            Sorters.SetSorting config.index >> config.fromSortersMsg
    in
    FilterV2.defaultFilter
        { openMsg = config.openMsg
        , closeMsg = config.discardMsg
        , editMsg = config.fromFiltersMsg << Filters.FilterMsg config.index
        , label = config.label
        , isOpen = config.isOpen
        , sortAscendingMsg = sortMsg SortAscending
        , sortDescendingMsg = sortMsg SortDescending
        , clearSortingMsg = config.fromSortersMsg Sorters.ClearSorting
        , alignRight = False
        }
        filter
        sorting
        |> FilterV2.withSize FilterV2.ExtraSmall
        |> FilterV2.renderElement renderConfig



-- Selectable reuses filter background


headerSelectToggle : RenderConfig -> msg -> Element msg
headerSelectToggle renderConfig toggleMsg =
    let
        headerAttrs =
            Element.onIndividualClick toggleMsg
                :: Primitives.roundedBorders contextSize
                :: Element.width Element.fill
                :: Element.padding ((34 - 16) // 2)
                :: Element.spacing 8
                :: Element.pointer
                :: Background.color Colors.gray200
                :: Element.mouseOver [ Background.color Colors.navyBlue200 ]
                :: Element.colorTransition 100
                ++ ARIA.toElementAttributes ARIA.roleButton
    in
    (localeTerms renderConfig |> .tables |> .selectAll)
        |> Icon.check
        |> Icon.withSize contextSize
        |> Icon.renderElement renderConfig
        |> Element.el headerAttrs
        |> Element.el
            [ Element.width (px 32) ]



-- Standard size used for headers


contextSize : Size
contextSize =
    Size.ExtraSmall
