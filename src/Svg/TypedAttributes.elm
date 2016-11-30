module Svg.TypedAttributes exposing (Length, Paint, Transform, cm, color, currentColor, cx, cy, em, ex, fill, fontFamily, fontSize, height, inch, large, larger, matrix, medium, mm, noPaint, num, opacity, paintRef, paintRefWithDefault, pc, percent, pt, px, r, rotate, scale, skewX, skewY, small, smaller, transform, translate, viewBox, width, x, xLarge, xSmall, xxLarge, xxSmall, y)

import Color exposing (Color)
import Svg exposing (Attribute)
import Svg.Attributes as Att


-- Units of length


type Length
    = Cm Float
    | Em Float
    | Ex Float
    | In Float
    | Mm Float
    | Num Float
    | Pc Float
    | Percent Float
    | Pt Float
    | Px Float


cm : Float -> Length
cm =
    Cm


em : Float -> Length
em =
    Em


ex : Float -> Length
ex =
    Ex


inch : Float -> Length
inch =
    In


lengthString : Length -> String
lengthString length =
    case length of
        Cm x ->
            toString x ++ "cm"

        Em x ->
            toString x ++ "em"

        Ex x ->
            toString x ++ "ex"

        In x ->
            toString x ++ "in"

        Mm x ->
            toString x ++ "mm"

        Num x ->
            toString x

        Pc x ->
            toString x ++ "pc"

        Percent x ->
            toString x ++ "%"

        Pt x ->
            toString x ++ "pt"

        Px x ->
            toString x ++ "px"


mm : Float -> Length
mm =
    Mm


num : Float -> Length
num =
    Num


pc : Float -> Length
pc =
    Pc


percent : Float -> Length
percent =
    Percent


pt : Float -> Length
pt =
    Pt


px : Float -> Length
px =
    Px



-- Transforms


type Transform
    = XForm String (List Float)


matrix : Float -> Float -> Float -> Float -> Float -> Float -> Transform
matrix a b c d e f =
    XForm "matrix" [ a, b, c, d, e, f ]


rotate : Float -> Float -> Float -> Transform
rotate a x y =
    XForm "rotate" [ a, x, y ]


scale : Float -> Float -> Transform
scale x y =
    XForm "scale" [ x, y ]


skewX : Float -> Transform
skewX x =
    XForm "skewX" [ x ]


skewY : Float -> Transform
skewY y =
    XForm "skewY" [ y ]


transform : List Transform -> Attribute a
transform =
    List.map transformString >> String.join " " >> Att.transform


translate : Float -> Float -> Transform
translate x y =
    XForm "translate" [ x, y ]


transformString : Transform -> String
transformString xform =
    case xform of
        XForm name args ->
            String.concat
                [ name
                , "("
                , String.join " " (List.map toString args)
                , ")"
                ]



-- Paint


type ExplicitPaint
    = NoPaint
    | CurrentColor
    | Color Color


type Paint
    = Explicit ExplicitPaint
    | Reference String ExplicitPaint


{-| Alpha values are ignored, since they are outside the SVG spec. If you need to specify opacity, you should use one of `opacity`, `stroke-opacity`, or `fill-opacity`.
-}
color : Color -> Paint
color =
    Color >> Explicit


currentColor : Paint
currentColor =
    Explicit CurrentColor


noPaint : Paint
noPaint =
    Explicit NoPaint


{-| If the given id is invalid, `noPaint` will be used as default.
-}
paintRef : String -> Paint
paintRef iri =
    Reference iri NoPaint


{-| If the id is invalid, the given color will be used.
-}
paintRefWithDefault : String -> Color -> Paint
paintRefWithDefault iri default =
    Reference iri (Color default)


paintString : Paint -> String
paintString paint =
    let
        explicitPaintString paint =
            case paint of
                NoPaint ->
                    "none"

                CurrentColor ->
                    "currentColor"

                Color c ->
                    let
                        rgb =
                            Color.toRgb c
                    in
                        String.concat
                            [ "rgb("
                            , toString rgb.red
                            , ","
                            , toString rgb.green
                            , ","
                            , toString rgb.blue
                            , ")"
                            ]
    in
        case paint of
            Explicit p ->
                explicitPaintString p

            Reference iri default ->
                String.concat
                    [ "url(#", iri, ") ", explicitPaintString default ]



-- Attributes


cx : Length -> Attribute a
cx =
    lengthString >> Att.cx


cy : Length -> Attribute a
cy =
    lengthString >> Att.cy


fill : Paint -> Attribute a
fill =
    paintString >> Att.fill


{-| An empty list will set `font-family: inherit`
-}
fontFamily : List String -> Attribute a
fontFamily families =
    case families of
        [] ->
            Att.fontFamily "inherit"

        _ ->
            Att.fontFamily (String.join ", " families)


{-| This function takes an explicit length. Absolute and relative font sizes (such as `small` or `larger` have their own functions.
-}
fontSize : Length -> Attribute a
fontSize =
    lengthString >> Att.fontSize


height : Length -> Attribute a
height =
    lengthString >> Att.height


large : Attribute a
large =
    Att.fontSize "large"


larger : Attribute a
larger =
    Att.fontSize "larger"


medium : Attribute a
medium =
    Att.fontSize "medium"


opacity : Float -> Attribute a
opacity =
    toString >> Att.opacity


r : Length -> Attribute a
r =
    lengthString >> Att.r


small : Attribute a
small =
    Att.fontSize "small"


smaller : Attribute a
smaller =
    Att.fontSize "smaller"


viewBox : Float -> Float -> Float -> Float -> Attribute a
viewBox minX minY width height =
    [ minX, minY, width, height ]
        |> List.map toString
        |> String.join " "
        |> Att.viewBox


width : Length -> Attribute a
width =
    lengthString >> Att.width


x : Length -> Attribute a
x =
    lengthString >> Att.x


xLarge : Attribute a
xLarge =
    Att.fontSize "x-large"


xSmall : Attribute a
xSmall =
    Att.fontSize "x-small"


xxLarge : Attribute a
xxLarge =
    Att.fontSize "xx-large"


xxSmall : Attribute a
xxSmall =
    Att.fontSize "xx-small"


y : Length -> Attribute a
y =
    lengthString >> Att.y
