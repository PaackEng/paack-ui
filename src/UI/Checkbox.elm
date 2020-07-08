module UI.Checkbox exposing
    ( Model, radioButton, fromCheckBoxes
    , CheckBoxes, checkBoxesEmpty, checkBox
    , Msg, update
    , toView, labelsEmpty, label
    , renderElement
    )

{-|


# Building model

@docs Model, radioButton, fromCheckBoxes


## Checkbox

@docs CheckBoxes, checkBoxesEmpty, checkBox


## Updating

@docs Msg, update


# Building view

@docs toView, labelsEmpty, label


# Rendering

@docs renderElement

-}

import Element exposing (Element)
import UI.Internal.Basics exposing (ifThenElse)
import UI.Internal.Checkbox as Internal
import UI.Internal.NArray as NArray exposing (NArray)
import UI.Internal.Size as Size
import UI.RenderConfig exposing (RenderConfig)
import UI.Size exposing (Size)
import UI.Utils.TypeNumbers as T


type Model qty
    = UniqueSelect (Maybe Int) -- Radiobuttons
    | MultipleSelect (CheckBoxes qty) -- Checkboxes


type alias CheckBoxes qty =
    NArray Bool qty


type Msg
    = Set Int Bool


type alias Labels qty =
    NArray String qty


type Checkbox qty msg
    = Checkbox (Msg -> msg) (Model qty) (Labels qty) Options


type alias Options =
    { size : Size }



-- Model


radioButton : Maybe Int -> Model qty
radioButton selected =
    UniqueSelect selected


checkBoxesEmpty : CheckBoxes T.Zero
checkBoxesEmpty =
    NArray.empty


checkBox : Bool -> CheckBoxes qty -> CheckBoxes (T.Increase qty)
checkBox selected model =
    NArray.push selected model


fromCheckBoxes : CheckBoxes qty -> Model qty
fromCheckBoxes boxes =
    MultipleSelect boxes



-- Component


toView : (Msg -> msg) -> Labels qty -> Model qty -> Checkbox qty msg
toView toExternalMsg labels model =
    Checkbox toExternalMsg model labels defaultOptions


defaultOptions : Options
defaultOptions =
    { size = Size.default }


labelsEmpty : Labels T.Zero
labelsEmpty =
    NArray.empty


label : String -> Labels qty -> Labels (T.Increase qty)
label value labels =
    NArray.push value labels



-- Update


update : Msg -> Model qty -> Model qty
update (Set index value) model =
    case model of
        UniqueSelect _ ->
            UniqueSelect <| ifThenElse value (Just index) Nothing

        MultipleSelect selectArray ->
            MultipleSelect <| NArray.set index value selectArray



-- Rendering


renderElement : RenderConfig -> Checkbox qty msg -> Element msg
renderElement renderConfig (Checkbox toExternalMsg model labels _) =
    case model of
        UniqueSelect selected ->
            -- TODO: Do it in NArray.foldl for O(n)
            labels
                |> NArray.indexedMap
                    (\index str ->
                        Internal.radioButton renderConfig
                            (Set index >> toExternalMsg)
                            str
                            (selected == Just index)
                    )
                |> NArray.toList
                |> Element.row []

        MultipleSelect selectedArr ->
            selectedArr
                |> NArray.toIndexedList
                |> List.map2
                    (\str ( index, selected ) ->
                        Internal.radioButton renderConfig
                            (Set index >> toExternalMsg)
                            str
                            selected
                    )
                    (NArray.toList labels)
                |> Element.row []
