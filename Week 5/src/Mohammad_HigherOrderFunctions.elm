module Mohammad_HigherOrderFunctions exposing (..)

import Html exposing (Html)

above100: Int -> Bool
above100 x =
    if x > 100 then
        True
    else
        False

customAbove: Int -> Int -> Bool
customAbove x y =
    if y > x then
        True
    else
        False

double: Int -> Int
double x = x * 2

even: Int -> Bool
even x =
    if modBy 2 x == 0 then 
        True
    else    
        False

myCollatz: List Int -> List Int
myCollatz list =
    case list of    
        [] ->
            []
        x::xs ->
            if even x then
                x//2 :: list
            else
                3*x+1 :: list

equal1: List Int -> Bool
equal1 list =
    case list of    
        [] ->
            True
        x :: xs ->
            if x == 1 then
                True
            else
                False

repeatUntil: (a -> Bool) -> (a -> a) -> a -> a
repeatUntil condition action number =
    if (condition number) == True then
        number   
    else
        repeatUntil condition action (action number)











my_results =
    [
        "Unit Tests",
        "\nrepeatUntil Function",
        "\nFor above100 and double 1",
        pr <| repeatUntil above100 double 1,
        "\nFor above100 and double 67",
        pr <| repeatUntil above100 double 67,
        "\nFor above100 and double 12",
        pr <| repeatUntil above100 double 12,
        "\nFor above100 and double 45",
        pr <| repeatUntil above100 double 45,
        "\nFor above100 and double 9",
        pr <| repeatUntil above100 double 9,

        "\nFor customAbove 5 and double 3",
        pr <| repeatUntil (customAbove 5) double 3,
        "\nFor customAbove 45 and double 9",
        pr <| repeatUntil (customAbove 45) double 6,
        "\nFor customAbove 12 and double 9",
        pr <| repeatUntil (customAbove 12) double 1,
        "\nFor customAbove 120 and double 20",
        pr <| repeatUntil (customAbove 120) double 20,
        "\nFor customAbove 123 and double 1",
        pr <| repeatUntil (customAbove 123) double 1,

        "\nFor equal1 and myCollatz [19]",
        pr <| repeatUntil equal1 myCollatz [19],
        "\nFor equal1 and myCollatz [81]",
        pr <| repeatUntil equal1 myCollatz [81],
        "\nFor equal1 and myCollatz [8]",
        pr <| repeatUntil equal1 myCollatz [8],
        "\nFor equal1 and myCollatz [54]",
        pr <| repeatUntil equal1 myCollatz [54],
        "\nFor equal1 and myCollatz [76]",
        pr <| repeatUntil equal1 myCollatz [76]
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