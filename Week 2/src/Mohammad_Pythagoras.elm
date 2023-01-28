module Mohammad_Pythagoras exposing (..)

import Html exposing (Html)

sqr: Int -> Int
sqr number =
    number^2

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


pythTriplesMap: List (Int, Int) -> List (Int, Int, Int)
pythTriplesMap list =
    --map through list using pythTriple method
    List.map pythTriple list

pythTriplesRec: List (Int, Int) -> List (Int, Int, Int)
pythTriplesRec list =
    case list of
        --checking if given list is empty
        [] ->
            --if true returns empty list and stop
            []
        --else split first element from the list
        x :: xs ->
            --using pythTriple in first element then do the recursion
            pythTriple x :: pythTriplesRec xs

arePythTriplesFilter: List (Int, Int, Int) -> List (Int, Int, Int)
arePythTriplesFilter list =
    --filter through list using isTripleTuple
    List.filter isTripleTuple list

arePythTriplesRec: List (Int, Int, Int) -> List (Int, Int, Int)
arePythTriplesRec list =
    case list of
        --checking if given list is empty
        [] ->
        --if true returns empty list and stop
            []
            --else split first element from the list
        x :: xs ->
            --checks if first element isTripleTuple
            if isTripleTuple x == True then
                --if true add to list with recursion of rest of list
                x :: arePythTriplesRec xs
            else
                --if false recursion rest of the list without adding first element
                arePythTriplesRec xs

my_results: List String
my_results =
    [
        "square",
        "square of 2 expecting 4",
            pr <| sqr 2,
        "square of 4 expecting 16",
            pr <| sqr 4,

        "\nisTriple",
        "with 3 4 5 expecting True",
            pr <| isTriple 3 4 5,
        "with 3 4 6 expecting False",
            pr <| isTriple 3 4 6,

        "\nleg1",
        "with 3 4 expecting 7",
            pr <| leg1 3 4,
        "with 5 6 expecting 11",
            pr <| leg1 5 6,

        "\nleg2",
        "with 3 4 expecting 24",
            pr <| leg2 3 4,
        "with 5 6 expecting 60",
            pr <| leg2 5 6,

        "\nhyp",
        "with 3 4 expecting 25",
            pr <| hyp 3 4,
        "with 5 6 expecting 61",
            pr <| hyp 5 6,

        "\npythTriple",
        "with 5 4 expecting (9, 40, 41)",
            pr <| pythTriple (5,4),
        "with 9 7 expecting (32, 126, 130)",
            pr <| pythTriple (9,7),

        "\nisTripleTuple",
        "with (3, 4, 6) expecting False",
            pr <| isTripleTuple (3,4,6),
        "with (5, 4) expecting True",
            pr <| isTripleTuple (pythTriple (5,4)),

        "\npythTriplesMap",
        "with [(5,4),(2,1),(35,7)] expecting [(9,40,41),(3,4,5),(1176,490,1274)]",
            pr <| pythTriplesMap [(5,4),(2,1),(35,7)],
        "\npythTriplesRec",
        "with [(5,4),(2,1),(35,7)] expecting [(9,40,41),(3,4,5),(1176,490,1274)] same as map",
            pr <| pythTriplesRec [(5,4),(2,1),(35,7)],

        "\narePythTriplesFilter",
        "with [(1,2,3), (9,40,41), (3,4,5), (100,2,500)] expecting [(9,40,41),(3,4,5)]",
            pr <| arePythTriplesFilter [(1,2,3), (9,40,41), (3,4,5), (100,2,500)],
        "\narePythTriplesRec",
        "with [(1,2,3), (9,40,41), (3,4,5), (100,2,500)] expecting [(9,40,41),(3,4,5)] same as filter",
            pr <| arePythTriplesRec [(1,2,3), (9,40,41), (3,4,5), (100,2,500)],

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