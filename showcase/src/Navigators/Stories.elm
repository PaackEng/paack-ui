module Navigators.Stories exposing (stories, update)

import UI.NavigationContainer as Nav
import Element
import Msg
import PluginOptions exposing (defaultWithoutMenu)
import Return as R exposing (Return)
import UI.Checkbox as Checkbox
import UI.Internal.Basics exposing (ifThenElse)
import UI.Text as Text
import UIExplorer exposing (storiesOf)
import Utils exposing (goToDocsCallToAction, iconsSvgSprite, prettifyElmCode, storyWithModel)

import Navigators.Model as Stories
import Navigators.Msg as Stories

update : Navigators.Msg -> Navigators.Model -> Return Navigators.Msg Navigators.Model
update msg model =
    case msg of
        Navigators.ForNav newValue ->

            let
                ( newState, subCmd ) =
                    Nav.stateUpdate subMsg model.navState
            in
            ( { model | state = newState }
            , Cmd.map Navigators.ForNav subCmd )


stories renderConfig =
    storiesOf
        "Checkboxes"
        [ navigatorDemo renderConfig ]


boxDemo renderConfig =
    let

        container  =
            { title = "Vrau" 
            , content = Nav.contentSingle Element.none
            , dialog = Nothing
            , hasMenu = False
            }
        view { navigatorsStories } =
            Nav.navigator ForNav
                navState
                (toNavContainer appConfig context >> Nav.containerMap ForPage)
                --|> Nav.withMenuPages (menuPageList appConfig.locale page)
                --|> Nav.withMenuActions
                    --[ Nav.menuAction
                        --(Icon.logout (t "sidebar.logout"))
                        --SessionClear
                    --]
                --|> Nav.withMenuLogo (t "common.paackSlogan")
                    --(Vectors.paackLogoWhite (Palette.color toneGray brightnessLight))
                |> Nav.toBrowserDocument appConfig.renderConfig page

    in
    storyWithModel
        ( "Default"
        , view
        , { defaultWithoutMenu
            | code = prettifyElmCode """
    Element.column [ Element.spacing 8 ]
        [ Checkbox.checkbox renderConfig
            Msg.Box1Set
            "Extra ketchup (Free)"
            model.box1
        , Checkbox.checkbox renderConfig
            Msg.Box2Set
            "Large french fries (+0.50 USD)"
            model.box2
        , Checkbox.checkbox renderConfig
            (\\newState -> ifThenElse newState Msg.Box3Check Msg.Box3Uncheck)
            "Heinz® Mayonnaise (+0.75 USD)"
            model.box3
        , ("Total: "
            ++ (String.fromFloat <| totalPrice model)
            ++ " USD"
          )
            |> Text.body1
            |> Text.renderElement renderConfig
        ]
"""
            , note = goToDocsCallToAction "Checkbox"
          }
        )
