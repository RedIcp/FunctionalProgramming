module Pythagoras exposing (..)

import Html exposing (Html)

sqr: Int -> Int
sqr x =
    x^2

isTriple: Int -> Int -> Int -> Bool
isTriple x y z =
    if sqr x + sqr y == sqr z then
    True
    else
    False

leg1 : Int -> Int -> Int
leg1 x y =
    if(x>y) then
    sqr x - sqr y
    else
    sqr y - sqr x

leg2 : Int -> Int -> Int
leg2 x y =
    2*x*y

hyp  : Int -> Int -> Int
hyp x y =
    sqr x + sqr y

pythTriple : (Int, Int) -> (Int, Int, Int)
pythTriple (x, y) =
    (leg1 x y, leg2 x y, hyp x y)

isTripleTuple: (Int, Int, Int) -> Bool
isTripleTuple (x, y, z) =
    isTriple x y z

my_results: List String
my_results =
    [
        "square",
        "square of 2",
        pr <| sqr 2,
        "square of 4",
        pr <| sqr 4,

        "\nisTriple",
        "with 3 4 5",
        pr <| isTriple 3 4 5,
        "with 3 4 6",
        pr <| isTriple 3 4 6,

        "\nleg1",
        "with 3 4",
        pr <| leg1 3 4,
        "with 5 6",
        pr <| leg1 5 6,

        "\nleg2",
        "with 3 4",
        pr <| leg2 3 4,
        "with 5 6",
        pr <| leg2 5 6,

        "\nhyp",
        "with 3 4",
        pr <| hyp 3 4,
        "with 5 6",
        pr <| hyp 5 6,

        "\npythTriple",
        "with 5 4",
        pr <| pythTriple (5,4),
        "with 9 7",
        pr <| pythTriple (9,7),

        "\nisTripleTuple",
        pr <| isTripleTuple (3,4,6),
        pr <| isTripleTuple (pythTriple (5,4)),
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