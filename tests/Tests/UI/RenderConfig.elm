module Tests.UI.RenderConfig exposing (tests)

import Expect
import Test exposing (..)
import UI.RenderConfig as RenderConfig exposing (RenderConfig)


tests : Test
tests =
    describe "UI.RenderConfig tests"
        [ ifMobile
        , noLongerMobile
        , ifVeryBig
        , ifVerySmall
        , ifPortrait
        , ifLandscape
        , ifSquare
        ]


ifMobile : Test
ifMobile =
    let
        window =
            { width = 599, height = 2000 }
    in
    describe "#ifMobile"
        [ test "this is the maximum size considered mobile" <|
            \_ ->
                window
                    |> init
                    |> RenderConfig.isMobile
                    |> Expect.equal True
        ]


noLongerMobile : Test
noLongerMobile =
    let
        window =
            { width = 600, height = 600 }
    in
    describe "#noLongerMobile"
        [ test "this is the mininum non-mobile size" <|
            \_ ->
                window
                    |> init
                    |> RenderConfig.isMobile
                    |> Expect.equal False
        ]


ifVeryBig : Test
ifVeryBig =
    let
        window =
            { width = 2560, height = 1080 }
    in
    describe "#ifVeryBig"
        [ test "this is not mobile" <|
            \_ ->
                window
                    |> init
                    |> RenderConfig.isMobile
                    |> Expect.equal False
        ]


ifVerySmall : Test
ifVerySmall =
    let
        window =
            { width = 240, height = 480 }
    in
    describe "#ifVerySmall"
        [ test "this is absolutely mobile" <|
            \_ ->
                window
                    |> init
                    |> RenderConfig.isMobile
                    |> Expect.equal True
        ]


ifPortrait : Test
ifPortrait =
    let
        window =
            { width = 240, height = 480 }
    in
    describe "#ifPortrait"
        [ test "this is portrait" <|
            \_ ->
                window
                    |> init
                    |> RenderConfig.isPortrait
                    |> Expect.equal True
        ]


ifLandscape : Test
ifLandscape =
    let
        window =
            { width = 1024, height = 768 }
    in
    describe "#ifLandscape"
        [ test "this is landscape" <|
            \_ ->
                window
                    |> init
                    |> RenderConfig.isPortrait
                    |> Expect.equal False
        ]


ifSquare : Test
ifSquare =
    let
        window =
            { width = 1024, height = 768 }
    in
    describe "#ifSquare"
        [ test "a square is landscape" <|
            \_ ->
                window
                    |> init
                    |> RenderConfig.isPortrait
                    |> Expect.equal False
        ]


init : { window | height : Int, width : Int } -> RenderConfig
init window =
    RenderConfig.init window RenderConfig.localeEnglish
