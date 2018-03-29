module Tests exposing (suite)

{-| Mainly to test round tripping results through color spaces.

Need to wait for 0.19 before it would work.

-}

import Color.Parser as Parser
import Color2 as Color exposing (Color)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


black =
    Color.rgb 0 0 0


white =
    Color.rgb 1 1 1


green =
    Color.rgb 0 1 0


suite : Test
suite =
    describe "Color Tests"
        [ rgbTest "#442233" 68 34 51
        , rgbTest "#0608bc" 6 8 188
        , rgbTest "#FaC" 255 170 204
        , rgbaTest "#44223343" 68 34 51 67
        , rgbaTest "#0608bc12" 6 8 188 18
        , rgbaTest "#FaC4" 255 170 204 68
        , rgbTest "rgb(23, 24, 12)" 23 24 12
        , rgbTest "rgb(50%, 15%, 56%)" 127.5 38.25 142.8
        , rgbaTest "rgba(23, 24, 12, 0.3)" 23 24 12 76.5
        , rgbaTest "rgba(50%, 15%, 56%, 80%)" 127.5 38.25 142.8 204
        , rgbTest "hsl(60, 23, 70)" 196 196 161
        , rgbTest "hsl(60%, 23%, 70%)" 161 175 196
        , rgbaTest "hsla(60, 23, 70, 0.4)" 196 196 161 102
        , rgbaTest "hsla(60%, 23%, 70%, 70%)" 161 175 196 178.5
        ]


rgbTest : String -> Float -> Float -> Float -> Test
rgbTest str r g b =
    test str <|
        \_ ->
            equalColor
                (Color.rgb (r / 255)
                    (g / 255)
                    (b / 255)
                )
                (parse str)


rgbaTest : String -> Float -> Float -> Float -> Float -> Test
rgbaTest str r g b a =
    test str <|
        \_ ->
            equalColor
                (Color.rgba (r / 255)
                    (g / 255)
                    (b / 255)
                    (a / 255)
                )
                (parse str)


equalColor : Color -> Color -> Expectation
equalColor c1 c2 =
    Expect.all
        [ \_ -> Expect.within (Expect.Absolute 0.001) c1.red c2.red
        , \_ -> Expect.within (Expect.Absolute 0.001) c1.green c2.green
        , \_ -> Expect.within (Expect.Absolute 0.001) c1.blue c2.blue
        , \_ -> Expect.within (Expect.Absolute 0.001) c1.alpha c2.alpha
        ]
        ()


parse : String -> Color
parse str =
    case Parser.css str of
        Just c ->
            c

        Nothing ->
            Debug.crash "Parse error"
