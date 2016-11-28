module Svg.TypedAttributes exposing (Length, Transform, cm, cx, cy, em, ex, height, inch, matrix, mm, num, opacity, pc, percent, pt, px, r, rotate, scale, skewX, skewY, transform, translate, width, x, y)

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



-- Attributes


cx : Length -> Attribute a
cx =
    lengthString >> Att.cx


cy : Length -> Attribute a
cy =
    lengthString >> Att.cy


height : Length -> Attribute a
height =
    lengthString >> Att.height


opacity : Float -> Attribute a
opacity =
    toString >> Att.opacity


r : Length -> Attribute a
r =
    lengthString >> Att.r


width : Length -> Attribute a
width =
    lengthString >> Att.width


x : Length -> Attribute a
x =
    lengthString >> Att.x


y : Length -> Attribute a
y =
    lengthString >> Att.y
