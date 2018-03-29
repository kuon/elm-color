module Color.Parser exposing (css, hex, hsl, hsla, rgb)

{-|

@docs css, rgb, rgba, hsl, hsla, hex

-}

import Color2 as Color exposing (Color)
import Regex


{-| Parse a string into a color.
The following formats will be tried in order:

  - hex
  - rgb
  - rgba
  - hsl
  - hsla

Read each function documentation for details

-}
css : String -> Maybe Color
css str =
    case hex str of
        Just c ->
            Just c

        Nothing ->
            case rgb str of
                Just c ->
                    Just c

                Nothing ->
                    case rgba str of
                        Just c ->
                            Just c

                        Nothing ->
                            case hsl str of
                                Just c ->
                                    Just c

                                Nothing ->
                                    case hsla str of
                                        Just c ->
                                            Just c

                                        Nothing ->
                                            Nothing


{-| Parse a string into a color.
The following formats is supported:

  - rgb(r, g, b)

  - r can be a number between 0 and 255 or a percentage 100% -> 255

  - g can be a number between 0 and 255 or a percentage 100% -> 255

  - b can be a number between 0 and 255 or a percentage 100% -> 255

Case insensitive, spaces are optional

-}
rgb : String -> Maybe Color
rgb str =
    let
        regex =
            Regex.regex "rgb\\s*\\(\\s*(\\d+%?)\\s*,\\s*(\\d+%?)\\s*\\,\\s*(\\d+%?)\\s*\\)"
                |> Regex.caseInsensitive

        n2i v =
            componentToFloat 255 v
    in
    case Regex.find (Regex.AtMost 1) regex str of
        [ { submatches } ] ->
            case submatches of
                [ Just r, Just g, Just b ] ->
                    Color.rgb (n2i r)
                        (n2i g)
                        (n2i b)
                        |> Just

                _ ->
                    Nothing

        _ ->
            Nothing


{-| Parse a string into a color.
The following formats is supported:

  - rgba(r, g, b, a)

  - r can be a number between 0 and 255 or a percentage 100% -> 255

  - g can be a number between 0 and 255 or a percentage 100% -> 255

  - b can be a number between 0 and 255 or a percentage 100% -> 255

  - a can be a number between 0 and 1 or a percentage 100% -> 1

Case insensitive, spaces are optional

-}
rgba : String -> Maybe Color
rgba str =
    let
        regex = Regex.regex "rgba\\s*\\(\\s*(\\d+%?)\\s*,\\s*(\\d+%?)\\s*\\,\\s*(\\d+%?)\\s*\\,\\s*(\\d+(\\.\\d+)?%?)\\s*\\)" |> Regex.caseInsensitive

        n2i v =
            componentToFloat 255 v
    in
    case Regex.find (Regex.AtMost 1) regex str of
        [ { submatches } ] ->
            case submatches of
                [ Just r, Just g, Just b, Just a, _ ] ->
                    Color.rgba (n2i r)
                        (n2i g)
                        (n2i b)
                        (componentToFloat 1 a)
                        |> Just
                _ ->
                    Nothing

        _ ->
            Nothing


{-| Parse a string into a color.
The following formats is supported:

  - hsl(h, s, l)

  - h can be a number between 0 and 360 or a percentage 100% -> 360

  - s can be a number between 0 and 100 or a percentage 100% -> 100

  - l can be a number between 0 and 100 or a percentage 100% -> 100

Case insensitive, spaces are optional

-}
hsl : String -> Maybe Color
hsl str =
    let
        regex =
            Regex.regex "hsl\\s*\\(\\s*(\\d+%?)\\s*,\\s*(\\d+%?)\\s*\\,\\s*(\\d+%?)\\s*\\)"
                |> Regex.caseInsensitive
    in
    case Regex.find (Regex.AtMost 1) regex str of
        [ { submatches } ] ->
            case submatches of
                [ Just h, Just s, Just l ] ->
                    Color.hsl (componentToFloat 360 h)
                        (componentToFloat 100 s)
                        (componentToFloat 100 l)
                        |> Just

                _ ->
                    Nothing

        _ ->
            Nothing


{-| Parse a string into a color.
The following formats is supported:

  - hsla(h, s, l, a)

  - h can be a number between 0 and 360 or a percentage 100% -> 360

  - s can be a number between 0 and 100 or a percentage 100% -> 100

  - l can be a number between 0 and 100 or a percentage 100% -> 100

  - a can be a number between 0 and 1 or a percentage 100% -> 1

Case insensitive, spaces are optional

-}
hsla : String -> Maybe Color
hsla str =
    let
        regex =
            Regex.regex
            "hsla\\s*\\(\\s*(\\d+%?)\\s*,\\s*(\\d+%?)\\s*\\,\\s*(\\d+%?)\\s*\\,\\s*(\\d+(\\.\\d+)?%?)\\s*\\)"
                |> Regex.caseInsensitive
    in
    case Regex.find (Regex.AtMost 1) regex str of
        [ { submatches } ] ->
            case submatches of
                [ Just h, Just s, Just l, Just a, _ ] ->
                    Color.hsla (componentToFloat 360 h)
                        (componentToFloat 100 s)
                        (componentToFloat 100 l)
                        (componentToFloat 1 a)
                        |> Just

                _ ->
                    Nothing

        _ ->
            Nothing


{-| Parse a string into a color.
The following formats are supported:

  - #fff RGB where each digit is doubled
  - #ffff RGBA where each digit is doubled
  - #ffffff RGB
  - #ffffffff RGBA

Case insensitive, hash is optional

-}
hex : String -> Maybe Color
hex str =
    let
        regexShort =
            Regex.regex "#([0-9a-f])([0-9a-f])([0-9a-f])([0-9a-f])?"
                |> Regex.caseInsensitive

        regex =
            Regex.regex "#([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})?"
                |> Regex.caseInsensitive

        h2i v =
            (hexByteToInt v |> toFloat) / 255
    in
    case Regex.find (Regex.AtMost 1) regex str of
        [ { submatches } ] ->
            case submatches of
                [ Just r, Just g, Just b, Just a ] ->
                    Color.rgba (h2i r)
                        (h2i g)
                        (h2i b)
                        (h2i a)
                        |> Just

                [ Just r, Just g, Just b, Nothing ] ->
                    Color.rgb (h2i r)
                        (h2i g)
                        (h2i b)
                        |> Just

                _ ->
                    Nothing

        _ ->
            case Regex.find (Regex.AtMost 1) regexShort str of
                [ { submatches } ] ->
                    case submatches of
                        [ Just r, Just g, Just b, Just a ] ->
                            Color.rgba (h2i (r ++ r))
                                (h2i (g ++ g))
                                (h2i (b ++ b))
                                (h2i (a ++ a))
                                |> Just

                        [ Just r, Just g, Just b, Nothing ] ->
                            Color.rgb (h2i (r ++ r))
                                (h2i (g ++ g))
                                (h2i (b ++ b))
                                |> Just

                        _ ->
                            Nothing

                _ ->
                    Nothing


{-| Component to int
-}
componentToFloat : Float -> String -> Float
componentToFloat factor str =
    case String.split "%" str of
        [ n ] ->
            Result.withDefault 0 (String.toFloat n) / factor

        [ n, _ ] ->
            Result.withDefault 0 (String.toFloat n) / 100

        _ ->
            0


{-| Hex 2 chars string to int
-}
hexByteToInt : String -> Int
hexByteToInt str =
    case String.split "" str of
        [ up, low ] ->
            16 * hexCharToInt up + hexCharToInt low

        _ ->
            0


{-| Hex 1 char string to int
-}
hexCharToInt : String -> Int
hexCharToInt str =
    case str of
        "1" ->
            1

        "2" ->
            2

        "3" ->
            3

        "4" ->
            4

        "5" ->
            5

        "6" ->
            6

        "7" ->
            7

        "8" ->
            8

        "9" ->
            9

        "a" ->
            10

        "A" ->
            10

        "b" ->
            11

        "B" ->
            11

        "c" ->
            12

        "C" ->
            12

        "d" ->
            13

        "D" ->
            13

        "e" ->
            14

        "E" ->
            14

        "f" ->
            15

        "F" ->
            15

        _ ->
            0
