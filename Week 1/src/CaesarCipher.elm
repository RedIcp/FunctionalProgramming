module CaesarCipher exposing (..)

import Html exposing (Html)

string2code: Char -> Int
string2code letter =
    Char.toCode letter

code2string: Int -> Char
code2string number =
    Char.fromCode number

subtract: Int -> Int -> Int
subtract x y =
    y-x

add: Int -> Int -> Int
add x y =
    x+y

shift: Int -> Int -> Int
shift x y =
    modBy 26 (add x y)

codeForUpperOrLower: Char -> Int
codeForUpperOrLower letter =
    if Char.isUpper letter then
    string2code 'A'
    else
    string2code 'a'


encode: Int -> Char -> Char
encode number letter =
    letter
        |> string2code
        |> subtract (codeForUpperOrLower letter)
        |> shift number
        |> add (codeForUpperOrLower letter)
        |> code2string


decode: Int -> Char -> Char
decode number letter =
    encode -number letter


my_results: List String
my_results =
    [
        "\nencode lowercase with x shifting 5",
        pr <| encode 5 'x',
        "\nencode uppercase with T shifting 7",
        pr <| encode 7 'T',

        "\ndecode uppercase with c shifting 5",
        pr <| decode 5 'c',
        "\ndecode uppercase with A shifting 7",
        pr <| decode 7 'A',
        "\n-- end --"
    ]



-- Boiler-plate below:

-- update this values for long output lines, or small browser windows
page_width = 80

to_wrap: String -> String
to_wrap my_value =
    if (String.length my_value <= page_width) then
        (String.left page_width my_value)
    else
        (String.left page_width my_value) ++ ("\n") ++ to_wrap (String.dropLeft page_width my_value)

to_div: String -> Html msg
to_div my_value =
    Html.div [] [(to_wrap my_value) |> Html.text]

pr = Debug.toString

main: Html msg
main = Html.pre
        []
        (List.map to_div my_results)