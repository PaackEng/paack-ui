module UI.PopUp exposing (PopUp, State, closable, collapsable, getPopUpType, initState, isCollapsed, remove, setCollapsed, toEl, withCollapsed, withContent, withTitle)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events exposing (onClick)
import Element.Font as Font
import Helpers exposing (ifThenElse)
import UI.Attributes exposing (ariaLabel, ariaRole, borderBottomWidth)
import UI.Icon as Icon
import UI.Theme as Theme


type PopUpStyle msg
    = Collapsable (Bool -> msg)
    | Closable msg


type alias Options msg =
    { popUpStyle : PopUpStyle msg
    , header : String
    , content : Element msg
    , collapsed : Bool
    }


type PopUp msg
    = PopUp (Options msg)


type State a
    = State
        { popUpType : a
        , collapsed : Bool
        }


initState : a -> Bool -> State a
initState popUpType collapsed =
    State
        { popUpType = popUpType
        , collapsed = collapsed
        }


isCollapsed : State a -> Bool
isCollapsed (State { collapsed }) =
    collapsed


getPopUpType : State a -> a
getPopUpType (State { popUpType }) =
    popUpType


setCollapsed : Bool -> State a -> State a
setCollapsed collapsed (State internal) =
    State { internal | collapsed = collapsed }


remove : State a -> List (State a) -> List (State a)
remove (State { popUpType }) popUps =
    popUps
        |> List.filter (\(State p) -> p.popUpType /= popUpType)


collapsable : (Bool -> msg) -> PopUp msg
collapsable onCollapseMsg =
    PopUp
        { popUpStyle = Collapsable onCollapseMsg
        , header = ""
        , content = el [] none
        , collapsed = True
        }


closable : msg -> PopUp msg
closable onCloseMsg =
    PopUp
        { popUpStyle = Closable onCloseMsg
        , header = ""
        , content = el [] none
        , collapsed = True
        }


withTitle : String -> PopUp msg -> PopUp msg
withTitle header (PopUp options) =
    PopUp { options | header = header }


withContent : Element msg -> PopUp msg -> PopUp msg
withContent content (PopUp options) =
    PopUp { options | content = content }


withCollapsed : Bool -> PopUp msg -> PopUp msg
withCollapsed collapsed (PopUp options) =
    PopUp { options | collapsed = collapsed }


toEl : PopUp msg -> Element msg
toEl (PopUp options) =
    let
        icon onClick iconType label =
            el
                [ alignRight
                , pointer
                , Font.color Theme.gray2
                , Events.onClick onClick
                , ariaRole "button"
                , ariaLabel label
                ]
                (iconType "")

        title =
            el
                [ alignLeft
                , Font.color Theme.gray2
                ]
                (text options.header)

        actionIcon =
            case options.popUpStyle of
                Closable onCloseMsg ->
                    icon onCloseMsg Icon.todo "pop-up-close"

                Collapsable onCollapseMsg ->
                    let
                        collapseMsg =
                            onCollapseMsg (not options.collapsed)

                        collapseIcon =
                            ifThenElse options.collapsed Icon.todo Icon.todo

                        label =
                            ifThenElse options.collapsed "pop-up-expand" "pop-up-collapse"
                    in
                    icon collapseMsg collapseIcon label

        header =
            row
                [ Border.color Theme.borderColor
                , borderBottomWidth 1
                , width fill
                , padding 16
                , Theme.smallSpacing
                , ariaRole "dialog"
                , ariaLabel "pop-up-header"
                , Theme.regular
                , Font.bold
                ]
                [ title
                , actionIcon
                ]

        content =
            if options.collapsed then
                el [ ariaLabel "pop-up-content" ] none

            else
                el
                    [ ariaLabel "pop-up-content"
                    , width (fill |> minimum 250)
                    , height fill
                    , paddingXY 20 30
                    ]
                    options.content
    in
    column
        [ Border.width 1
        , Border.color Theme.borderColor
        , Border.roundEach { topLeft = 5, topRight = 5, bottomLeft = 0, bottomRight = 0 }
        , Background.color Theme.white
        , Theme.borderShadow
        , width fill
        , ariaRole "dialog"
        , ariaLabel "pop-up"
        ]
        [ header
        , content
        ]
