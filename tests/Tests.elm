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
        , testTransforms
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


testTransforms : Test
testTransforms =
    describe "transforms"
        [ test "affine mattrix" <|
            \() ->
                Expect.equal
                    (transform [ matrix 1 2 3 4 5 6 ])
                    (Att.transform "matrix(1 2 3 4 5 6)")
        , test "translation" <|
            \() ->
                Expect.equal
                    (transform [ translate 2 -3 ])
                    (Att.transform "translate(2 -3)")
        , test "rotation" <|
            \() ->
                Expect.equal
                    (transform [ rotate 0.5 1 1 ])
                    (Att.transform "rotate(0.5 1 1)")
        , test "scale" <|
            \() ->
                Expect.equal
                    (transform [ scale 3 4 ])
                    (Att.transform "scale(3 4)")
        , test "skewX" <|
            \() ->
                Expect.equal
                    (transform [ skewX 4 ])
                    (Att.transform "skewX(4)")
        , test "skewY" <|
            \() ->
                Expect.equal
                    (transform [ skewY 3 ])
                    (Att.transform "skewY(3)")
        , test "multiple transforms" <|
            \() ->
                Expect.equal
                    (transform [ rotate 1 2 3, scale -2 0.1 ])
                    (Att.transform "rotate(1 2 3) scale(-2 0.1)")
        ]
