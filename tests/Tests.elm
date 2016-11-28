module Tests exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (list, int, tuple, string)
import Svg exposing (Attribute)
import Svg.TypedAttributes exposing (..)
import Svg.Attributes as Att


all : Test
all =
    describe "Building attributes"
        [ testLengthAttrEqual "width" width Att.width
        , testLengthAttrEqual "height" height Att.height
        , testLengthAttrEqual "x" x Att.x
        , testLengthAttrEqual "y" y Att.y
        , testLengthAttrEqual "cx" cx Att.cx
        , testLengthAttrEqual "cy" cy Att.cy
        , testLengthAttrEqual "r" r Att.r
        , testNumberAttrEqual "opacity" opacity Att.opacity
        ]


testLengthAttrEqual : String -> (Length -> Attribute a) -> (String -> Attribute a) -> Test
testLengthAttrEqual name typed untyped =
    describe name
        [ test "no unit" <|
            \() ->
                Expect.equal
                    (typed (num 12.1))
                    (untyped "12.1")
        , test "em" <|
            \() ->
                Expect.equal
                    (typed (em 12.1))
                    (untyped "12.1em")
        , test "ex" <|
            \() ->
                Expect.equal
                    (typed (ex 12.1))
                    (untyped "12.1ex")
        , test "px" <|
            \() ->
                Expect.equal
                    (typed (px 12.1))
                    (untyped "12.1px")
        , test "in" <|
            \() ->
                Expect.equal
                    (typed (inch 12.1))
                    (untyped "12.1in")
        , test "cm" <|
            \() ->
                Expect.equal
                    (typed (cm 12.1))
                    (untyped "12.1cm")
        , test "mm" <|
            \() ->
                Expect.equal
                    (typed (mm 12.1))
                    (untyped "12.1mm")
        , test "pt" <|
            \() ->
                Expect.equal
                    (typed (pt 12.1))
                    (untyped "12.1pt")
        , test "pc" <|
            \() ->
                Expect.equal
                    (typed (pc 12.1))
                    (untyped "12.1pc")
        , test "percent" <|
            \() ->
                Expect.equal
                    (typed (percent 12.1))
                    (untyped "12.1%")
        ]


testNumberAttrEqual : String -> (Float -> Attribute a) -> (String -> Attribute a) -> Test
testNumberAttrEqual name typed untyped =
    test name <|
        \() ->
            Expect.equal
                (typed 12.1)
                (untyped "12.1")
