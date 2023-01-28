module Mohammad_ModellingMathFunction exposing (..)

import Html exposing (Html)

type Function
  = Power  Function Int
  | Mult  Function Function
  | Div   Function Function
  | Plus  Function Function
  | Minus Function Function 
  | Const Int
  | X

print: Function -> String
print operator =
    case operator of
        X ->
            "x"
        Const number ->
            String.fromInt number
        Minus func1 func2 ->
            "(" ++ (print func1) ++ "-" ++ (print func2) ++ ")"
        Plus func1 func2 ->
            "(" ++ (print func1) ++ "+" ++ (print func2) ++ ")"
        Mult func1 func2 ->
            "(" ++ (print func1) ++ "*" ++ (print func2) ++ ")"
        Div func1 func2 ->
            "(" ++ (print func1) ++ "/" ++ (print func2) ++ ")"
        Power func1 number ->
            "(" ++ (print func1) ++ "^" ++ String.fromInt number ++ ")"

eval: Float -> Function -> Float
eval value operator =
    case operator of
        X ->
            value
        Const number ->
            toFloat number
        Minus func1 func2 ->
            (eval value func1) - (eval value func2)
        Plus func1 func2 ->
            (eval value func1) + (eval value func2)
        Mult func1 func2 ->
            (eval value func1) * (eval value func2)
        Div func1 func2 ->
            (eval value func1) / (eval value func2)
        Power func1 number ->
            (eval value func1) ^ toFloat number

printYaxis: Int -> Int -> Int -> List String
printYaxis yMin yMax value =
    -- List.foldr will go through from y minimum to y maximum
    List.foldr(\y acc -> 
        -- if y is less the the given value
        if (y <= value) then
            -- then * will be added
            "*" :: acc
        else
            -- else - will be added
            "-" :: acc) [] (List.range yMin yMax)

graph: Function -> Int -> Int -> Int -> Int -> List String
graph function xMin xMax yMin yMax =
    -- List.foldr will go through from x minimum to x maximum
    List.foldr(\x acc ->
        -- if value y for value of x is less than y maximum
        -- eval function gives value of y
        if (eval (toFloat x) function) >= (toFloat xMin) && (eval (toFloat x) function) <= (toFloat yMax) then
            String.join "" (printYaxis yMin yMax (round (eval (toFloat x) function))) :: acc
        else
            acc) [] (List.range xMin xMax)
    
-- (((3)+(x))*((x)-x^5))+(2)
f = Plus (Mult (Plus (Const 3) X) (Minus X (Power X 5))) (Const 2)

-- (((x/5 - 1) ^ 4) - (((x/-2) + 2) ^ 2)) + 6
g = Plus (Minus (Power (Minus (Div (X)(Const 5))(Const 1))(4))(Power (Plus (Div (X)(Const -2))(Const 2))(2))) (Const 6)

derivative: Function -> Function
derivative operator =
    case operator of
        X ->
            Const 1
        Const number ->
            Const 0
        Minus func1 func2 ->
            Minus (derivative func1) (derivative func2)
        Plus func1 func2 ->
            Plus (derivative func1) (derivative func2)
        Mult func1 func2 ->
            Plus (Mult (derivative func1) func2) (Mult func1 (derivative func2))
        Div func1 func2 ->
            Div (Minus (Mult func2 (derivative func1)) (Mult func1 (derivative func2))) (Power func2 2)
        Power func1 number ->
            Mult (Const number) (Power func1 (number-1))

simplify: Function -> Function
simplify operator =
    case operator of
        X ->
            X
        Const number ->
            Const number
        Minus func1 func2 ->
            if simplify func1 == Const 0 then
                simplify func2
            else if simplify func2 == Const 0 then
                simplify func1
            else
                Minus (simplify func1) (simplify func2)
        Plus func1 func2 ->
            if simplify func1 == Const 0 then
                simplify func2
            else if simplify func2 == Const 0 then
                simplify func1
            else
                Plus (simplify func1) (simplify func2)
        Mult func1 func2 ->
            if simplify func1 == Const 0 || simplify func2 == Const 0 then
                Const 0
            else if simplify func1 == Const 1 then
                simplify func2
            else if simplify func2 == Const 1 then
                simplify func1
            else
                Mult (simplify func1) (simplify func2)
        Div func1 func2 ->
            if simplify func1 == Const 0 then
                Const 0
            else
                Div (simplify func1) (simplify func2)
        Power func1 number ->
            if number == 0 then
                Const 0
            else if number == 1 then
                simplify func1
            else    
                Power (simplify func1) number


my_results =
    [
        "Unit Tests",

        "\nprint Function",
        "\nFor f = Plus (Mult (Plus (Const 3) X) (Minus X (Power X 5))) (Const 2)",
        pr <| print f,
        "\nFor g = Plus (Minus (Power (Minus (Div (X)(Const 5))(Const 1))(4))(Power (Plus (Div (X)(Const -2))(Const 2))(2))) (Const 6)",
        pr <| print g,

        "\neval Function",
        "\nFor f = (((3+x)*(x-(x^5)))+2) with value 2",
        pr <| eval 2 f,
        "\nFor g = (((((x/5)-1)^4)-(((x/-2)+2)^2))+6) with value 3",
        pr <| eval 3 g,


        "\nUnit Tests",
        "\nderivative Function",
        "\nFor f = (((3+x)*(x-(x^5)))+2)",
        pr <| print <| derivative f,
        "\nFor g = (((((x/5)-1)^4)-(((x/-2)+2)^2))+6)",
        pr <| print <| derivative g,

        "\nsimplify Function",
        "\nFor ((((0+1)*(x-(x^5)))+((3+x)*(1-(5*(x^4)))))+0)",
        pr <| print <| simplify <| derivative f,
        "\nFor (((4*(((x/5)-1)^3))-(2*(((x/-2)+2)^1)))+0)",
        pr <| print <| simplify <| derivative g
        -- "\ngraph for g",
        -- pr <| graph g -10 20 -33 33
    ]















-- Boiler-plate below:

-- update this values for long output lines, or small browser windows
page_width = 70

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