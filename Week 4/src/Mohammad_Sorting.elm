module Mohammad_Sorting exposing (..)

import Html exposing (Html)

halve : List comparable -> ( List comparable, List comparable )
halve list =
  case list of
    -- if the list is empty
    [] ->
        -- just return empty list
      ( [], [] )

    x :: xs ->
      let
        -- making tuple of two half lists
        (firstList, secondList) =
        -- recursing the rest
          halve xs
      in
        -- it is always going to concat with second Tuple
        -- thats why they switched postion everytime
        -- therefore if one element is added to first list
        -- on next iteration one element is gonna be added to second list
        ( secondList, x :: firstList )

merge : List comparable -> List comparable -> List comparable
merge listOne listTwo =
    case listOne of
        -- if the listOne is empty
        [] ->
            -- returns listTwo because there might be remaining listTwo element
            listTwo
        x :: xs ->
            case listTwo of
                -- if the listTwo is empty
                [] ->
                    -- returns listOne because there are remaining listOne elements
                    listOne
                y :: ys ->
                    -- if first listOne element is lower than first listTwo element
                    if x < y then
                        -- listOne elements goes first
                        x :: merge xs listTwo
                    else
                        -- else listTwo elemnets goes first
                        y :: merge listOne ys

msort : List comparable -> List comparable
msort list =
  case list of
    [] ->
      list
    -- if list is not empty but has one element
    [_] ->
        list
    -- if list has more than one element
    _ ->
      let
        -- spliting the list into two and assigning them two tuples
          (halfOne, halfTwo) =
            halve list
      in
        -- merging recursion of first half with recursion of second half
        merge (msort halfOne) (msort halfTwo)


my_results =
    [
        "Unit Tests",

        "\nhalve Function",
        "\nFor [1,3,4,5,5,5,7,2,3]",        
        pr <| halve [1,3,4,5,5,5,7,2,3],
        "\nFor [1,56,16,52,61,6,2,1]",
        pr <| halve [1,56,16,52,61,6,2,1],
        "\nFor [9,6,3,69,85,58,55,4,5]",
        pr <| halve [9,6,3,69,85,58,55,4,5],
        "\nFor [51,2,6,5,8,26,5,8,25]",
        pr <| halve [51,2,6,5,8,26,5,8,25],
        "\nFor [8,5,2,2,24,14,9,6,3]",
        pr <| halve [8,5,2,2,24,14,9,6,3],

        "\nmerge Function",
        "\nFor [3,5,5,2] [1,4,5,7,3]",
        pr <| merge [3,5,5,2] [1,4,5,7,3],
        "\nFor [56,52,6,1] [1,16,61,2]",
        pr <| merge [56,52,6,1] [1,16,61,2],
        "\nFor [6,69,58,4] [9,3,85,55,5]",
        pr <| merge [6,69,58,4] [9,3,85,55,5],
        "\nFor [2,5,26,8] [51,6,8,5,25]",
        pr <| merge [2,5,26,8] [51,6,8,5,25],
        "\nFor [5,2,14,6] [8,2,24,9,3]",
        pr <| merge [5,2,14,6] [8,2,24,9,3],

        "\nmSort Function",
        "\nFor [1,3,4,5,5,5,7,2,3]",
        pr <| msort [1,3,4,5,5,5,7,2,3],
        "\nFor [1,56,16,52,61,6,2,1]",
        pr <| msort [1,56,16,52,61,6,2,1],
        "\nFor [9,6,3,69,85,58,55,4,5]",
        pr <| msort [9,6,3,69,85,58,55,4,5],
        "\nFor [51,2,6,5,8,26,5,8,25]",
        pr <| msort [51,2,6,5,8,26,5,8,25],
        "\nFor [8,5,2,2,24,14,9,6,3]",
        pr <| msort [8,5,2,2,24,14,9,6,3]
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