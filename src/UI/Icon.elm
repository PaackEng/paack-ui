module UI.Icon exposing
    ( svgSpriteImport
    , Icon
    , addCircle, add, arrowCurveLeft, arrowDown, arrowLeft, arrowRight, arrowShapeUp, arrowUp, bell, bicycle, bike, bluetooth, boltDisabled, bolt, boxFilled, boxOutlined, boxesFilled, boxesOutlined, calendar, camera, car, cellular, checkmarkCircle, checkmarkRoundedRectangle, checkmark, chevronDown, chevronLeft, chevronRight, chevronUp, circle, clockIssue, clockLocked, clock, closeCircle, closeRoundedRectangle, close, coins, collapse, crosshair, currencyNote, directions, download, eBike, eCar, eVan, edit, ellipsis, emailFill, emailOutline, empty, expand, eyeHide, eye, filter, firstPage, fix, folder, food, frozen, gVan, grocery, groups, hamburger, hand, home, hourglass, info, keys, lastPage, legacyApis, legacyHistorical, legacyLabelPrinter, legacyReport, legacyRetailerDashboard, legacySignOut, list, location, lock, logout, map, mapPinRadius, mapPin, messageLock, messageOTP, message, microphoneMute, microphone, minus, move, notepad, notes, notification, otp, personAssign, personAssigned, personError, personRemove, personUnassign, person, personsAdd, personsNotApplicable, persons, phoneEndCall, phoneStartCall, placeholder, printerError, printer, redo, reload, remove, report, reporting, reset, return, scanBarcode, scanGroup, scanPackage, scanSpace, scan, search, settings, shelves, sort, spaceError, spaceSearch, space, speakerMute, speaker, store, tag, trash, truck, undo, van, walk, warning, waves, webhook
    , fixing, flag, loader
    , assignPerson, boxes, check, configure, delete, done, eventLog, fixIssues, insert, moreActions, nextContent, notifications, paackSpaces, packages, phone, pause, previousContent, print, sandwichMenu, searchSpace, seeMore, sortDecreasing, sortIncreasing, success, toggle, toggleDown, toggleUp, unassignPerson, wait
    , getHint
    , withColor
    , withSize, withCustomSize
    , renderElement
    )

{-| `UI.Icon` is an implementation of icons using an SVG-spritesheet.

To use these icons, first, you must insert the spritesheet once (and only once) in the layout.
The sprite sheet injection uses a custom `Html` component that later populates using a parcel's import.
See [`Icon.svgSpriteImport`](UI-Icon#svgSpriteImport) to know what to do on the Elm's side.
Parcel's instructions are in [README](https://github.com/PaackEng/paack-ui/blob/master/README.md).

**NOTE**: The spritesheet is automatically injected when using [`UI.Document.toBrowserDocument`](UI-Document#toBrowserDocument)

An icon can be created and rendered as in the following pipeline:

    Icon.logout "Logout from this account"
        |> Icon.renderElement renderConfig


# Embed

@docs svgSpriteImport


# Building

@docs Icon


## Current names

@docs addCircle, add, arrowCurveLeft, arrowDown, arrowLeft, arrowRight, arrowShapeUp, arrowUp, bell, bicycle, bike, bluetooth, boltDisabled, bolt, boxFilled, boxOutlined, boxesFilled, boxesOutlined, calendar, camera, car, cellular, checkmarkCircle, checkmarkRoundedRectangle, checkmark, chevronDown, chevronLeft, chevronRight, chevronUp, circle, clockIssue, clockLocked, clock, closeCircle, closeRoundedRectangle, close, coins, collapse, crosshair, currencyNote, directions, download, eBike, eCar, eVan, edit, ellipsis, emailFill, emailOutline, empty, expand, eyeHide, eye, filter, firstPage, fix, folder, food, frozen, gVan, grocery, groups, hamburger, hand, home, hourglass, info, keys, lastPage, legacyApis, legacyHistorical, legacyLabelPrinter, legacyReport, legacyRetailerDashboard, legacySignOut, list, location, lock, logout, map, mapPinRadius, mapPin, messageLock, messageOTP, message, microphoneMute, microphone, minus, move, notepad, notes, notification, otp, personAssign, personAssigned, personError, personRemove, personUnassign, person, personsAdd, personsNotApplicable, persons, phoneEndCall, phoneStartCall, placeholder, printerError, printer, redo, reload, remove, report, reporting, reset, return, scanBarcode, scanGroup, scanPackage, scanSpace, scan, search, settings, shelves, sort, spaceError, spaceSearch, space, speakerMute, speaker, store, tag, trash, truck, undo, van, walk, warning, waves, webhook


## Special

@docs fixing, loader


## Old (depreacted) names

@docs assignPerson, boxes, check, configure, delete, done, eventLog, fixIssues, insert, moreActions, nextContent, notifications, paackSpaces, packages, phone, pause, previousContent, print, sandwichMenu, searchSpace, seeMore, sortDecreasing, sortIncreasing, success, toggle, toggleDown, toggleUp, unassignPerson, wait


# Disassemble

@docs getHint


# Color

@docs withColor


# Size

@docs withSize, withCustomSize


# Rendering

@docs renderElement

-}

import Element exposing (..)
import Element.Font as Font
import Html
import Svg exposing (Svg)
import Svg.Attributes as SvgAttrs
import UI.Internal.Size as Size exposing (Size)
import UI.Internal.Svg as Svg
import UI.Palette as Palette
import UI.RenderConfig exposing (RenderConfig)
import UI.Utils.ARIA as ARIA


type alias Properties =
    { hint : String
    , glyph : String
    }


type alias Options =
    { color : IconColor
    , size : Int
    , spin : Bool
    , notification : Bool
    }


type IconColor
    = ColorFromPalette Palette.Color
    | ColorInherit


{-| The `Icon` type is used for describing the component for later rendering.
-}
type Icon
    = Icon Properties Options



-- Options


{-| Icons colors can variate to match text color or contrast with a background.
See [`Palette.color`](UI-Palette#color) and [`Palette.setContrasting`](UI-Palette#setContrasting) for how to compose a valid color value.

    Icon.search "Search logs"
        |> Icon.withColor (Palette.color Palette.tonePrimary Palette.brightnessMiddle)
        |> Icon.renderElement renderConfig

-}
withColor : Palette.Color -> Icon -> Icon
withColor color (Icon prop opt) =
    Icon prop { opt | color = ColorFromPalette color }


{-| With `Icon.withSize`, you'll be able to scale the icon between the [standard sizes][size].

[size]: UI-Size

    Icon.withSize Size.large someIcon

**NOTE**: Default value is [`Size.medium`](UI-Size#medium).

-}
withSize : Size -> Icon -> Icon
withSize size icon =
    withCustomSize (sizeToInt size) icon


{-| With `Icon.withCustomSize`, you'll be able to scale the icon using an integer value.

All Icons are constraint to fit inside a 1:1 square.
So the set value also coincides with this square's side length.
E.g., setting `Icon.withCustomSize 48` will produce a square with 48px on each side.

    Icon.withCustomSize 48 someIcon

**NOTE**: Default value is 20.

-}
withCustomSize : Int -> Icon -> Icon
withCustomSize size (Icon prop opt) =
    Icon prop { opt | size = size }



-- Icons


defaultInit : String -> String -> Icon
defaultInit glyph hint =
    Icon (Properties hint glyph) defaultOptions



-- Current names


{-| Icon constructor.

    Icon.addCircle "Accessibility hint"

-}
addCircle : String -> Icon
addCircle =
    defaultInit "Add-Circle"


{-| Icon constructor.

    Icon.add "Accessibility hint"

-}
add : String -> Icon
add =
    defaultInit "Add"


{-| Icon constructor.

    Icon.arrowCurveLeft "Accessibility hint"

-}
arrowCurveLeft : String -> Icon
arrowCurveLeft =
    defaultInit "Arrow-CurveLeft"


{-| Icon constructor.

    Icon.arrowDown "Accessibility hint"

-}
arrowDown : String -> Icon
arrowDown =
    defaultInit "Arrow-Down"


{-| Icon constructor.

    Icon.arrowLeft "Accessibility hint"

-}
arrowLeft : String -> Icon
arrowLeft =
    defaultInit "Arrow-Left"


{-| Icon constructor.

    Icon.arrowRight "Accessibility hint"

-}
arrowRight : String -> Icon
arrowRight =
    defaultInit "Arrow-Right"


{-| Icon constructor.

    Icon.arrowShapeUp "Accessibility hint"

-}
arrowShapeUp : String -> Icon
arrowShapeUp =
    defaultInit "Arrow-Shape-Up"


{-| Icon constructor.

    Icon.arrowUp "Accessibility hint"

-}
arrowUp : String -> Icon
arrowUp =
    defaultInit "Arrow-Up"


{-| Icon constructor.

    Icon.bell "Accessibility hint"

-}
bell : String -> Icon
bell =
    defaultInit "Bell"


{-| Icon constructor.

    Icon.bicycle "Accessibility hint"

-}
bicycle : String -> Icon
bicycle =
    defaultInit "Bicycle"


{-| Icon constructor.

    Icon.bike "Accessibility hint"

-}
bike : String -> Icon
bike =
    defaultInit "Bike"


{-| Icon constructor.

    Icon.bluetooth "Accessibility hint"

-}
bluetooth : String -> Icon
bluetooth =
    defaultInit "Bluetooth"


{-| Icon constructor.

    Icon.boltDisabled "Accessibility hint"

-}
boltDisabled : String -> Icon
boltDisabled =
    defaultInit "Bolt-Disabled"


{-| Icon constructor.

    Icon.bolt "Accessibility hint"

-}
bolt : String -> Icon
bolt =
    defaultInit "Bolt"


{-| Icon constructor.

    Icon.boxFilled "Accessibility hint"

-}
boxFilled : String -> Icon
boxFilled =
    defaultInit "Box-Filled"


{-| Icon constructor.

    Icon.boxOutlined "Accessibility hint"

-}
boxOutlined : String -> Icon
boxOutlined =
    defaultInit "Box-Outlined"


{-| Icon constructor.

    Icon.boxesFilled "Accessibility hint"

-}
boxesFilled : String -> Icon
boxesFilled =
    defaultInit "Boxes-Filled"


{-| Icon constructor.

    Icon.boxesOutlined "Accessibility hint"

-}
boxesOutlined : String -> Icon
boxesOutlined =
    defaultInit "Boxes-Outlined"


{-| Icon constructor.

    Icon.calendar "Accessibility hint"

-}
calendar : String -> Icon
calendar =
    defaultInit "Calendar"


{-| Icon constructor.

    Icon.camera "Accessibility hint"

-}
camera : String -> Icon
camera =
    defaultInit "Camera"


{-| Icon constructor.

    Icon.car "Accessibility hint"

-}
car : String -> Icon
car =
    defaultInit "Car"


{-| Icon constructor.

    Icon.cellular "Accessibility hint"

-}
cellular : String -> Icon
cellular =
    defaultInit "Cellular"


{-| Icon constructor.

    Icon.checkmarkCircle "Accessibility hint"

-}
checkmarkCircle : String -> Icon
checkmarkCircle =
    defaultInit "Checkmark-Circle"


{-| Icon constructor.

    Icon.checkmarkRoundedRectangle "Accessibility hint"

-}
checkmarkRoundedRectangle : String -> Icon
checkmarkRoundedRectangle =
    defaultInit "Checkmark-RoundedRectangle"


{-| Icon constructor.

    Icon.checkmark "Accessibility hint"

-}
checkmark : String -> Icon
checkmark =
    defaultInit "Checkmark"


{-| Icon constructor.

    Icon.chevronDown "Accessibility hint"

-}
chevronDown : String -> Icon
chevronDown =
    defaultInit "Chevron-Down"


{-| Icon constructor.

    Icon.chevronLeft "Accessibility hint"

-}
chevronLeft : String -> Icon
chevronLeft =
    defaultInit "Chevron-Left"


{-| Icon constructor.

    Icon.chevronRight "Accessibility hint"

-}
chevronRight : String -> Icon
chevronRight =
    defaultInit "Chevron-Right"


{-| Icon constructor.

    Icon.chevronUp "Accessibility hint"

-}
chevronUp : String -> Icon
chevronUp =
    defaultInit "Chevron-Up"


{-| Icon constructor.

    Icon.circle "Accessibility hint"

-}
circle : String -> Icon
circle =
    defaultInit "Circle"


{-| Icon constructor.

    Icon.clockIssue "Accessibility hint"

-}
clockIssue : String -> Icon
clockIssue =
    defaultInit "Clock-Issue"


{-| Icon constructor.

    Icon.clockLocked "Accessibility hint"

-}
clockLocked : String -> Icon
clockLocked =
    defaultInit "Clock-Locked"


{-| Icon constructor.

    Icon.clock "Accessibility hint"

-}
clock : String -> Icon
clock =
    defaultInit "Clock"


{-| Icon constructor.

    Icon.closeCircle "Accessibility hint"

-}
closeCircle : String -> Icon
closeCircle =
    defaultInit "Close-Circle"


{-| Icon constructor.

    Icon.closeRoundedRectangle "Accessibility hint"

-}
closeRoundedRectangle : String -> Icon
closeRoundedRectangle =
    defaultInit "Close-RoundedRectangle"


{-| Icon constructor.

    Icon.close "Accessibility hint"

-}
close : String -> Icon
close =
    defaultInit "Close"


{-| Icon constructor.

    Icon.coins "Accessibility hint"

-}
coins : String -> Icon
coins =
    defaultInit "Coins"


{-| Icon constructor.

    Icon.collapse "Accessibility hint"

-}
collapse : String -> Icon
collapse =
    defaultInit "Collapse"


{-| Icon constructor.

    Icon.crosshair "Accessibility hint"

-}
crosshair : String -> Icon
crosshair =
    defaultInit "Crosshair"


{-| Icon constructor.

    Icon.currencyNote "Accessibility hint"

-}
currencyNote : String -> Icon
currencyNote =
    defaultInit "Currency-Note"


{-| Icon constructor.

    Icon.directions "Accessibility hint"

-}
directions : String -> Icon
directions =
    defaultInit "Directions"


{-| Icon constructor.

    Icon.download "Accessibility hint"

-}
download : String -> Icon
download =
    defaultInit "Download"


{-| Icon constructor.

    Icon.eBike "Accessibility hint"

-}
eBike : String -> Icon
eBike =
    defaultInit "E-Bike"


{-| Icon constructor.

    Icon.eCar "Accessibility hint"

-}
eCar : String -> Icon
eCar =
    defaultInit "E-Car"


{-| Icon constructor.

    Icon.eVan "Accessibility hint"

-}
eVan : String -> Icon
eVan =
    defaultInit "E-Van"


{-| Icon constructor.

    Icon.edit "Accessibility hint"

-}
edit : String -> Icon
edit =
    defaultInit "Edit"


{-| Icon constructor.

    Icon.ellipsis "Accessibility hint"

-}
ellipsis : String -> Icon
ellipsis =
    defaultInit "Ellipsis"


{-| Icon constructor.

    Icon.emailFill "Accessibility hint"

-}
emailFill : String -> Icon
emailFill =
    defaultInit "Email-Fill"


{-| Icon constructor.

    Icon.emailOutline "Accessibility hint"

-}
emailOutline : String -> Icon
emailOutline =
    defaultInit "Email-Outline"


{-| Icon constructor.

    Icon.empty "Accessibility hint"

-}
empty : String -> Icon
empty =
    defaultInit "Empty"


{-| Icon constructor.

    Icon.expand "Accessibility hint"

-}
expand : String -> Icon
expand =
    defaultInit "Expand"


{-| Icon constructor.

    Icon.eyeHide "Accessibility hint"

-}
eyeHide : String -> Icon
eyeHide =
    defaultInit "Eye-Hide"


{-| Icon constructor.

    Icon.eye "Accessibility hint"

-}
eye : String -> Icon
eye =
    defaultInit "Eye"


{-| Icon constructor.

    Icon.filter "Accessibility hint"

-}
filter : String -> Icon
filter =
    defaultInit "Filter"


{-| Icon constructor.

    Icon.firstPage "Accessibility hint"

-}
firstPage : String -> Icon
firstPage =
    defaultInit "First-Page"


{-| Icon constructor.

    Icon.fix "Accessibility hint"

-}
fix : String -> Icon
fix =
    defaultInit "Fix"

{-| Icon constructor.

    Icon.flag "Accessibility hint"

-}
flag : String -> Icon
flag =
    defaultInit "Flag"

{-| Icon constructor.

    Icon.folder "Accessibility hint"

-}
folder : String -> Icon
folder =
    defaultInit "Folder"


{-| Icon constructor.

    Icon.food "Accessibility hint"

-}
food : String -> Icon
food =
    defaultInit "Food"


{-| Icon constructor.

    Icon.frozen "Accessibility hint"

-}
frozen : String -> Icon
frozen =
    defaultInit "Frozen"


{-| Icon constructor.

    Icon.gVan "Accessibility hint"

-}
gVan : String -> Icon
gVan =
    defaultInit "G-Van"


{-| Icon constructor.

    Icon.grocery "Accessibility hint"

-}
grocery : String -> Icon
grocery =
    defaultInit "Grocery"


{-| Icon constructor.

    Icon.groups "Accessibility hint"

-}
groups : String -> Icon
groups =
    defaultInit "Groups"


{-| Icon constructor.

    Icon.hamburger "Accessibility hint"

-}
hamburger : String -> Icon
hamburger =
    defaultInit "Hamburger"


{-| Icon constructor.

    Icon.hand "Accessibility hint"

-}
hand : String -> Icon
hand =
    defaultInit "Hand"


{-| Icon constructor.

    Icon.home "Accessibility hint"

-}
home : String -> Icon
home =
    defaultInit "Home"


{-| Icon constructor.

    Icon.hourglass "Accessibility hint"

-}
hourglass : String -> Icon
hourglass =
    defaultInit "Hourglass"


{-| Icon constructor.

    Icon.info "Accessibility hint"

-}
info : String -> Icon
info =
    defaultInit "Info"


{-| Icon constructor.

    Icon.keys "Accessibility hint"

-}
keys : String -> Icon
keys =
    defaultInit "Keys"


{-| Icon constructor.

    Icon.lastPage "Accessibility hint"

-}
lastPage : String -> Icon
lastPage =
    defaultInit "Last-Page"


{-| Icon constructor.

    Icon.legacyApis "Accessibility hint"

-}
legacyApis : String -> Icon
legacyApis =
    defaultInit "Legacy-Apis"


{-| Icon constructor.

    Icon.legacyHistorical "Accessibility hint"

-}
legacyHistorical : String -> Icon
legacyHistorical =
    defaultInit "Legacy-Historical"


{-| Icon constructor.

    Icon.legacyLabelPrinter "Accessibility hint"

-}
legacyLabelPrinter : String -> Icon
legacyLabelPrinter =
    defaultInit "Legacy-LabelPrinter"


{-| Icon constructor.

    Icon.legacyReport "Accessibility hint"

-}
legacyReport : String -> Icon
legacyReport =
    defaultInit "Legacy-Report"


{-| Icon constructor.

    Icon.legacyRetailerDashboard "Accessibility hint"

-}
legacyRetailerDashboard : String -> Icon
legacyRetailerDashboard =
    defaultInit "Legacy-RetailerDashboard"


{-| Icon constructor.

    Icon.legacySignOut "Accessibility hint"

-}
legacySignOut : String -> Icon
legacySignOut =
    defaultInit "Legacy-SignOut"


{-| Icon constructor.

    Icon.list "Accessibility hint"

-}
list : String -> Icon
list =
    defaultInit "List"


{-| Icon constructor.

    Icon.location "Accessibility hint"

-}
location : String -> Icon
location =
    defaultInit "Location"


{-| Icon constructor.

    Icon.lock "Accessibility hint"

-}
lock : String -> Icon
lock =
    defaultInit "Lock"


{-| Icon constructor.

    Icon.logout "Accessibility hint"

-}
logout : String -> Icon
logout =
    defaultInit "Logout"


{-| Icon constructor.

    Icon.map "Accessibility hint"

-}
map : String -> Icon
map =
    defaultInit "Map"


{-| Icon constructor.

    Icon.mapPinRadius "Accessibility hint"

-}
mapPinRadius : String -> Icon
mapPinRadius =
    defaultInit "MapPin-Radius"


{-| Icon constructor.

    Icon.mapPin "Accessibility hint"

-}
mapPin : String -> Icon
mapPin =
    defaultInit "MapPin"


{-| Icon constructor.

    Icon.messageLock "Accessibility hint"

-}
messageLock : String -> Icon
messageLock =
    defaultInit "Message-Lock"


{-| Icon constructor.

    Icon.messageOTP "Accessibility hint"

-}
messageOTP : String -> Icon
messageOTP =
    defaultInit "Message-OTP"


{-| Icon constructor.

    Icon.message "Accessibility hint"

-}
message : String -> Icon
message =
    defaultInit "Message"


{-| Icon constructor.

    Icon.microphoneMute "Accessibility hint"

-}
microphoneMute : String -> Icon
microphoneMute =
    defaultInit "Microphone-Mute"


{-| Icon constructor.

    Icon.microphone "Accessibility hint"

-}
microphone : String -> Icon
microphone =
    defaultInit "Microphone"


{-| Icon constructor.

    Icon.minus "Accessibility hint"

-}
minus : String -> Icon
minus =
    defaultInit "Minus"


{-| Icon constructor.

    Icon.move "Accessibility hint"

-}
move : String -> Icon
move =
    defaultInit "Move"


{-| Icon constructor.

    Icon.notepad "Accessibility hint"

-}
notepad : String -> Icon
notepad =
    defaultInit "Notepad"


{-| Icon constructor.

    Icon.notes "Accessibility hint"

-}
notes : String -> Icon
notes =
    defaultInit "Notes"


{-| Icon constructor.

    Icon.notification "Accessibility hint"

-}
notification : String -> Icon
notification =
    defaultInit "Notification"


{-| Icon constructor.

    Icon.otp "Accessibility hint"

-}
otp : String -> Icon
otp =
    defaultInit "OTP"


{-| Icon constructor.

    Icon.personAssign "Accessibility hint"

-}
personAssign : String -> Icon
personAssign =
    defaultInit "Person-Assign"


{-| Icon constructor.

    Icon.personAssigned "Accessibility hint"

-}
personAssigned : String -> Icon
personAssigned =
    defaultInit "Person-Assigned"


{-| Icon constructor.

    Icon.personError "Accessibility hint"

-}
personError : String -> Icon
personError =
    defaultInit "Person-Error"


{-| Icon constructor.

    Icon.personRemove "Accessibility hint"

-}
personRemove : String -> Icon
personRemove =
    defaultInit "Person-Remove"


{-| Icon constructor.

    Icon.personUnassign "Accessibility hint"

-}
personUnassign : String -> Icon
personUnassign =
    defaultInit "Person-Unassign"


{-| Icon constructor.

    Icon.person "Accessibility hint"

-}
person : String -> Icon
person =
    defaultInit "Person"


{-| Icon constructor.

    Icon.personsAdd "Accessibility hint"

-}
personsAdd : String -> Icon
personsAdd =
    defaultInit "Persons-Add"


{-| Icon constructor.

    Icon.personsNotApplicable "Accessibility hint"

-}
personsNotApplicable : String -> Icon
personsNotApplicable =
    defaultInit "Persons-NotApplicable"


{-| Icon constructor.

    Icon.persons "Accessibility hint"

-}
persons : String -> Icon
persons =
    defaultInit "Persons"


{-| Icon constructor.

    Icon.phoneEndCall "Accessibility hint"

-}
phoneEndCall : String -> Icon
phoneEndCall =
    defaultInit "Phone-EndCall"


{-| Icon constructor.

    Icon.phoneStartCall "Accessibility hint"

-}
phoneStartCall : String -> Icon
phoneStartCall =
    defaultInit "Phone-StartCall"


{-| Icon constructor.

    Icon.placeholder "Accessibility hint"

-}
placeholder : String -> Icon
placeholder =
    defaultInit "Placeholder"


{-| Icon constructor.

    Icon.printerError "Accessibility hint"

-}
printerError : String -> Icon
printerError =
    defaultInit "Printer-Error"


{-| Icon constructor.

    Icon.printer "Accessibility hint"

-}
printer : String -> Icon
printer =
    defaultInit "Printer"


{-| Icon constructor.

    Icon.redo "Accessibility hint"

-}
redo : String -> Icon
redo =
    defaultInit "Redo"


{-| Icon constructor.

    Icon.reload "Accessibility hint"

-}
reload : String -> Icon
reload =
    defaultInit "Reload"


{-| Icon constructor.

    Icon.remove "Accessibility hint"

-}
remove : String -> Icon
remove =
    defaultInit "Remove"


{-| Icon constructor.

    Icon.report "Accessibility hint"

-}
report : String -> Icon
report =
    defaultInit "Report"


{-| Icon constructor.

    Icon.reporting "Accessibility hint"

-}
reporting : String -> Icon
reporting =
    defaultInit "Reporting"


{-| Icon constructor.

    Icon.reset "Accessibility hint"

-}
reset : String -> Icon
reset =
    defaultInit "Reset"


{-| Icon constructor.

    Icon.return "Accessibility hint"

-}
return : String -> Icon
return =
    defaultInit "Return"


{-| Icon constructor.

    Icon.scanBarcode "Accessibility hint"

-}
scanBarcode : String -> Icon
scanBarcode =
    defaultInit "Scan-Barcode"


{-| Icon constructor.

    Icon.scanGroup "Accessibility hint"

-}
scanGroup : String -> Icon
scanGroup =
    defaultInit "Scan-Group"


{-| Icon constructor.

    Icon.scanPackage "Accessibility hint"

-}
scanPackage : String -> Icon
scanPackage =
    defaultInit "Scan-Package"


{-| Icon constructor.

    Icon.scanSpace "Accessibility hint"

-}
scanSpace : String -> Icon
scanSpace =
    defaultInit "Scan-Space"


{-| Icon constructor.

    Icon.scan "Accessibility hint"

-}
scan : String -> Icon
scan =
    defaultInit "Scan"


{-| Icon constructor.

    Icon.search "Accessibility hint"

-}
search : String -> Icon
search =
    defaultInit "Search"


{-| Icon constructor.

    Icon.settings "Accessibility hint"

-}
settings : String -> Icon
settings =
    defaultInit "Settings"


{-| Icon constructor.

    Icon.shelves "Accessibility hint"

-}
shelves : String -> Icon
shelves =
    defaultInit "Shelves"


{-| Icon constructor.

    Icon.sort "Accessibility hint"

-}
sort : String -> Icon
sort =
    defaultInit "Sort"


{-| Icon constructor.

    Icon.spaceError "Accessibility hint"

-}
spaceError : String -> Icon
spaceError =
    defaultInit "Space-Error"


{-| Icon constructor.

    Icon.spaceSearch "Accessibility hint"

-}
spaceSearch : String -> Icon
spaceSearch =
    defaultInit "Space-Search"


{-| Icon constructor.

    Icon.space "Accessibility hint"

-}
space : String -> Icon
space =
    defaultInit "Space"


{-| Icon constructor.

    Icon.speakerMute "Accessibility hint"

-}
speakerMute : String -> Icon
speakerMute =
    defaultInit "Speaker-Mute"


{-| Icon constructor.

    Icon.speaker "Accessibility hint"

-}
speaker : String -> Icon
speaker =
    defaultInit "Speaker"


{-| Icon constructor.

    Icon.store "Accessibility hint"

-}
store : String -> Icon
store =
    defaultInit "Store"


{-| Icon constructor.

    Icon.tag "Accessibility hint"

-}
tag : String -> Icon
tag =
    defaultInit "Tag"


{-| Icon constructor.

    Icon.trash "Accessibility hint"

-}
trash : String -> Icon
trash =
    defaultInit "Trash"


{-| Icon constructor.

    Icon.truck "Accessibility hint"

-}
truck : String -> Icon
truck =
    defaultInit "Truck"


{-| Icon constructor.

    Icon.undo "Accessibility hint"

-}
undo : String -> Icon
undo =
    defaultInit "Undo"


{-| Icon constructor.

    Icon.van "Accessibility hint"

-}
van : String -> Icon
van =
    defaultInit "Van"


{-| Icon constructor.

    Icon.walk "Accessibility hint"

-}
walk : String -> Icon
walk =
    defaultInit "Walk"


{-| Icon constructor.

    Icon.warning "Accessibility hint"

-}
warning : String -> Icon
warning =
    defaultInit "Warning"


{-| Icon constructor.

    Icon.waves "Accessibility hint"

-}
waves : String -> Icon
waves =
    defaultInit "Waves"


{-| Icon constructor.

    Icon.webhook "Accessibility hint"

-}
webhook : String -> Icon
webhook =
    defaultInit "Webhook"



-- Special ones


{-| A monkey wrench with a red ball.

    Icon.fixing "Fix something"

-}
fixing : String -> Icon
fixing hint =
    Icon
        (Properties hint "Fix")
        { defaultOptions | notification = True }


{-| A loading spinner.

    Icon.loader "You spin me right 'round"

-}
loader : String -> Icon
loader hint =
    Icon
        (Properties hint "Loader")
        { defaultOptions | spin = True }



-- Deprecated names


{-| A person with a plus sign on the bottom-right.

    Icon.assingPerson "Select Manager"

-}
assignPerson : String -> Icon
assignPerson =
    personAssign


{-| Piled up boxes.

    Icon.boxes "Stacked boxed"

**NOTE**: Deprecated name.

-}
boxes : String -> Icon
boxes =
    boxesFilled


{-| A check mark, commonly used inside checkboxes and radio buttons.

    Icon.check "Notepad"

**NOTE**: Deprecated name.

-}
check : String -> Icon
check =
    checkmark


{-| A gear. Usually used for opening settings managers.

    Icon.configure "Open display settings"

**NOTE**: Deprecated name.

-}
configure : String -> Icon
configure =
    settings


{-| A trash can with an "x" on it.

    Icon.delete "Trash item"

**NOTE**: Deprecated name.

-}
delete : String -> Icon
delete =
    trash


{-| A notepad with a checkmark.

    Icon.done "Mark all as done."

**NOTE**: Deprecated name.

-}
done : String -> Icon
done =
    notepad


{-| A chat-like baloon.
In Paack's apps this symbolizes the log of actions.

    Icon.eventLog "Events"

**NOTE**: Deprecated name.

-}
eventLog : String -> Icon
eventLog =
    message


{-| A thunder bolt.

    Icon.fixIssues "Fix issues from selected groups"

**NOTE**: Deprecated name.

-}
fixIssues : String -> Icon
fixIssues =
    bolt


{-| A plus sign within a circle.

    Icon.insert "Insert row"

**NOTE**: Deprecated name.

-}
insert : String -> Icon
insert =
    addCircle


{-| Three-dot in series horizontally.
It's usually used in the web to access less commonly used actions.

    Icon.moreActions "More actions"

**NOTE**: Deprecated name.

-}
moreActions : String -> Icon
moreActions =
    ellipsis


{-| An arrow pointing to the right used in chevrons and paginators.

    Icon.nextContent "Next page"

**NOTE**: Deprecated name.

-}
nextContent : String -> Icon
nextContent =
    chevronRight


{-| A bell for indicating notifications.

    Icon.notifications "See notifications"

**NOTE**: Deprecated name.

-}
notifications : String -> Icon
notifications =
    bell


{-| Internal jargon in Paack.

    Icon.paackSpaces "Spaces"

**NOTE**: Deprecated name.

-}
paackSpaces : String -> Icon
paackSpaces =
    space


{-| "Box-Outlined".

    Icon.packages "Order's packages"

**NOTE**: Deprecated name.

-}
packages : String -> Icon
packages =
    boxOutlined


{-| A phone.

    Icon.phone "Call"

**NOTE**: Deprecated name.

-}
phone : String -> Icon
phone =
    phoneStartCall


{-| The hand from the stop sign.

    Icon.pause "Paused orders"

**NOTE**: Deprecated name.

-}
pause : String -> Icon
pause =
    hand


{-| An arrow pointing to the left used in chevrons and paginators.

    Icon.previousContent "Previous page"

**NOTE**: Deprecated name.

-}
previousContent : String -> Icon
previousContent =
    chevronLeft


{-| An A4 ink printer.
Indicates the availability to print something related to the surrounding content.

    Icon.print "Printer pacakage's barcode"

**NOTE**: Deprecated name.

-}
print : String -> Icon
print =
    printer


{-| Tree-bar stacked vertically.
It's usually used in mobile to toggle left bar menus.

    Icon.sandwichMenu "Open pages menu"

**NOTE**: Deprecated name.

-}
sandwichMenu : String -> Icon
sandwichMenu =
    hamburger


{-| The space icon with a search icon in the right-bottom.

    Icon.searchSpace "Search Space"

**NOTE**: Deprecated name.

-}
searchSpace : String -> Icon
searchSpace =
    spaceSearch


{-| Ellipsis (three-dots horizontally aligned).
For showing hidden details.

    Icon.seeMore "Read more about this article"

**NOTE**: Deprecated name.

-}
seeMore : String -> Icon
seeMore =
    ellipsis


{-| An arrow pointing up.

    Icon.sortDecreasing "Sort from Z to A"

**NOTE**: Deprecated name.

-}
sortDecreasing : String -> Icon
sortDecreasing =
    arrowUp


{-| An arrow pointing down.

    Icon.sortIncreasing "Sort from A to Z"

**NOTE**: Deprecated name.

-}
sortIncreasing : String -> Icon
sortIncreasing =
    arrowDown


{-| A check mark in circle, commonly used to indicate a success state.

    Icon.success "Success"

**NOTE**: Deprecated name.

-}
success : String -> Icon
success =
    checkmarkCircle


{-| A foldable paper, toggle some content between showing/hiding, or full/collapsed.

    Icon.toggle "Expand technical details"

**NOTE**: Deprecated name.

-}
toggle : String -> Icon
toggle =
    map


{-| An arrow pointing down.
May indicate the expansion of a hidden content below.

    Icon.toggleDown "Expand details"

**NOTE**: Deprecated name.

-}
toggleDown : String -> Icon
toggleDown =
    chevronDown


{-| An arrow pointing up.
May indicate the collapsing of the content below.

    Icon.toggleUp "Collapse details"

**NOTE**: Deprecated name.

-}
toggleUp : String -> Icon
toggleUp =
    chevronUp


{-| A person with a plus sign on the bottom-right.

    Icon.assingPerson "Select Manager"

**NOTE**: Deprecated name.

-}
unassignPerson : String -> Icon
unassignPerson =
    personUnassign


{-| An hourglass.

    Icon.wait "On hold"

**NOTE**: Deprecated name.

-}
wait : String -> Icon
wait =
    hourglass



-- Rendering


{-| End of the builder's life.
The result of this function is a ready-to-insert Elm UI's Element.
-}
renderElement : RenderConfig -> Icon -> Element msg
renderElement _ (Icon { hint, glyph } opt) =
    let
        staticAttrs =
            (ARIA.toElementAttributes <| ARIA.roleImage hint)
                ++ [ Element.centerX
                   , Font.center
                   , Element.width <| Element.px opt.size
                   , Element.height <| Element.px opt.size
                   ]

        attrs =
            case opt.color of
                ColorFromPalette realColor ->
                    (realColor
                        |> Palette.toElementColor
                        |> Font.color
                    )
                        :: staticAttrs

                ColorInherit ->
                    staticAttrs
    in
    Element.el attrs <|
        if opt.notification then
            notificationSvgIcon opt glyph

        else
            svgIcon opt glyph



-- Disassemble


{-| For not creating data duplication, `Icon.getHint` can extract the hint from an `Icon`.

    let
        icon =
            Icon.seeMore "Read more about this"
    in
    Element.row []
        [ Icon.renderElement renderConfig icon
        , Icon.getHint icon
            |> Text.body1
            |> Text.renderElement renderConfig
        ]

-}
getHint : Icon -> String
getHint (Icon { hint } _) =
    hint



-- Layout


{-| Imports the SVG-spritesheet with all icons into the rendered HTML.

There is no need for using this function when you're using [`UI.Document`][nav], and you should be using it.
But, in case you aren't, you need to insert this function on the most top component, which probably is the [`Element.layout`][layout], like this:

[nav]: UI-Document
[layout]: /packages/mdgriffith/elm-ui/latest/Element#layout

    main : Program Flags Model Msg
    main =
        { yourProgram
            | view =
                { title = yourPageTitle
                , body =
                    [ Icon.svgSpriteImport
                    , Element.layout someAttributes yourPageView
                    ]
                }
        }

**NOTE**: Use [`UI.Document`][nav]!

-}
svgSpriteImport : Html.Html msg
svgSpriteImport =
    Html.div []
        [ Html.node "paack-svg-icon-sprite" [] []
        , circleMask
        ]



-- Internals


defaultOptions : Options
defaultOptions =
    { color = ColorInherit
    , notification = False
    , size = sizeToInt Size.default
    , spin = False
    }


svgIcon : Options -> String -> Element msg
svgIcon opt iconId =
    svgToHtml [ useIcon opt iconId ]


notificationSvgIcon : Options -> String -> Element msg
notificationSvgIcon opt iconId =
    svgToHtml
        [ Svg.g [ SvgAttrs.mask "url(#icon-circle-mask)" ]
            [ Svg.rect
                [ SvgAttrs.x "0"
                , SvgAttrs.y "0"
                , SvgAttrs.width "100%"
                , SvgAttrs.height "100%"
                , SvgAttrs.fill "transparent"
                ]
                []
            , useIcon opt iconId
            ]
        , Svg.circle
            [ SvgAttrs.fill <| Palette.toCssColor Palette.red700
            , SvgAttrs.r "17%"
            , SvgAttrs.cx "75%"
            , SvgAttrs.cy "20%"
            ]
            []
        ]


svgToHtml : List (Svg msg) -> Element msg
svgToHtml =
    Element.html
        << Svg.svg
            [ SvgAttrs.width "100%"
            , SvgAttrs.height "100%"
            , SvgAttrs.fill "currentColor"
            ]


useIcon : Options -> String -> Svg msg
useIcon { size, spin } iconId =
    Svg.use
        [ SvgAttrs.id iconId
        , SvgAttrs.xlinkHref ("#" ++ iconId)
        ]
    <|
        if spin then
            let
                center =
                    toFloat size / 2
            in
            [ Svg.animateSpin center center ]

        else
            []


sizeToInt : Size -> Int
sizeToInt size =
    case size of
        Size.Large ->
            40

        Size.Medium ->
            32

        Size.Small ->
            24

        Size.ExtraSmall ->
            16


circleMask : Svg msg
circleMask =
    Svg.svg
        [ SvgAttrs.version "1.1"
        , SvgAttrs.style "position:absolute"
        , SvgAttrs.width "0"
        , SvgAttrs.height "0"
        ]
        [ Svg.defs
            []
            [ Svg.mask
                [ SvgAttrs.id "icon-circle-mask"
                , SvgAttrs.maskUnits "objectBoundingBox"
                , SvgAttrs.maskContentUnits "objectBoundingBox"
                ]
                [ Svg.rect
                    [ SvgAttrs.fill "white"
                    , SvgAttrs.x "0"
                    , SvgAttrs.y "0"
                    , SvgAttrs.height "1"
                    , SvgAttrs.width "1"
                    ]
                    []
                , Svg.circle
                    [ SvgAttrs.fill "black"
                    , SvgAttrs.r "0.21"
                    , SvgAttrs.cx "0.75"
                    , SvgAttrs.cy "0.20"
                    ]
                    []
                ]
            ]
        ]
