module Mohammad_CaesarCipher exposing (..)

import Html exposing (Html)

-- WEEK 1

string2code: Char -> Int
string2code letter =
    Char.toCode letter

code2string: Int -> Char
code2string number =
    Char.fromCode number

subtract: Int -> Int -> Int
subtract firstNumber secondNumber =
    secondNumber-firstNumber

add: Int -> Int -> Int
add firstNumber secondNumber =
    firstNumber+secondNumber

shift: Int -> Int -> Int
shift characterNumber stepsShifted =
    modBy 26 (add characterNumber stepsShifted)

codeForUpperOrLower: Char -> Int
codeForUpperOrLower letter =
    if Char.isUpper letter then
        string2code 'A'
    else
        string2code 'a'


encode: Int -> Char -> Char
encode stepsToBeShifted letter =
    letter
        |> string2code
        |> subtract (codeForUpperOrLower letter)
        |> shift stepsToBeShifted
        |> add (codeForUpperOrLower letter)
        |> code2string


decode: Int -> Char -> Char
decode stepsToBeShifted letter =
    encode -stepsToBeShifted letter

string2list: String -> List Char
string2list string =
    String.toList string

list2string: List Char -> String
list2string list =
    String.fromList list


-- WEEK 2

--checking if char is letter
isLetter: Char -> Bool
isLetter character =
    Char.isAlpha character

--removing all non letter
normalize : String -> String
normalize string=
    --checking after uncons
    case String.uncons string of
        --if nothing then returns empty and break
        Nothing ->
            ""
        Just (head, tail) ->
            --if head contains letter
            if isLetter head then
                --if true then add and do recursion
                String.cons head (normalize tail)
            else
                --if false just do recursion leaving non letter
                normalize tail

--main function
encrypt: Int -> String -> String
encrypt stepsToBeShifted string =
    --checking after uncons
    case String.uncons string of
        --if nothing then returns empty and break
        Nothing ->
            ""
        Just (head, tail) ->
            --encode head then recursion
            String.cons (encode stepsToBeShifted head) (encrypt stepsToBeShifted tail)

--main function
decrypt: Int -> String -> String
decrypt stepsToBeShifted string =
    --same as encrypt with negative shift
    encrypt -stepsToBeShifted string

-- WEEK 3

startsWith: String -> String -> Bool
startsWith search target =
    -- unconcatenating search string first
    case String.uncons search of
        Nothing ->
            -- if no string left it means all other is matched
            True
        Just (headSearch, tailSearch) ->
            -- unconcatenating target string second
            case String.uncons target of
                Nothing ->
                    -- if target string ends before search string it means it does not match
                    False
                Just (headTarget, tailTarget) ->
                    if headSearch == headTarget then
                        -- if first element matches then recursion happens to check rest
                        startsWith tailSearch tailTarget
                    else
                        False

contains: String -> String -> Bool
contains search target =
    if startsWith search target == True then
        -- if target starts with search string it means it contains
        True
    else
        case String.uncons target of
            Nothing ->
                -- if target string ends before search string it means it does not match
                False
            Just (head, tail) ->
                -- check the rest
                contains search tail


containsList: List String -> String -> Bool
containsList searchList target =
    case searchList of
        [] ->
            -- if there is not search string left it mean it does not match
            False
        x :: xs ->
            if contains x target == True then
                True
            else
                -- check rest of the search string options
                containsList xs target



candidates: List String -> String -> List (Int, String)
candidates searchList target =
    List.foldr(\x acc ->
        if containsList searchList (decrypt x target) == True then
            -- concatenating matches
            (x, decrypt x target) :: acc
        else
            -- going through 25 shifts
            acc) [] (List.range 1 25)
    



my_results: List String
my_results =
    [
        "\nencode lowercase with x shifting 5 expecting c",
            pr <| encode 5 'x',
        "\nencode uppercase with T shifting 7 expecting A",
            pr <| encode 7 'T',

        "\ndecode uppercase with c shifting 5 expecting x",
            pr <| decode 5 'c',
        "\ndecode uppercase with A shifting 7 expecting T",
            pr <| decode 7 'A',

        "\nremoving non letter and spaces from (Hello, Fontys!) expecting HelloFontys",
            pr <| normalize "Hello, Fontys!",

        "\nencrypt (Hello, Fontys!) by shifting 7 expecting OlssvMvuafz",
            pr <| encrypt 7 (normalize "Hello, Fontys!"),

        "\ndecrypt (OlssvMvuafz) by shifting 7 expecting HelloFontys",
            pr <| decrypt 7 (normalize "OlssvMvuafz"),

        "\nUnit Tests",
        "\nstartsWith Function",
            "Yel in Yellow",
            pr <| startsWith "Yel" "Yellow",
            "Bla in Bla",
            pr <| startsWith "Bla" "Bla",
            "Elsoow in Blsdadaa",
            pr <| startsWith "Elsoow" "Blsdadaa",
            "BlueIsWater in BlueIsWaterasadw",
            pr <| startsWith "BlueIsWater" "BlueIsWaterasadw",
            "elow in thssf",
            pr <| startsWith "elow" "thssf",

        "\ncontains Function",
            "ell in Yellow",
            pr <| contains "ell" "Yellow",
            "Blac in Black",
            pr <| contains "Blac" "Black",
            "IsWat in BlueIsWaterasadw",
            pr <| contains "IsWat" "BlueIsWaterasadw",
            "Blac in Blsdadaa",
            pr <| contains "Blac" "Blsdadaa",
            "aw in Wgsdsdcs",
            pr <| contains "aw" "Wgsdsdcs",

        "\ncontainsList Function",
            "[ell, ow] in Yellow",
            pr <| containsList ["ell", "ow"] "Yellow",
            "[Blc, ks] in Black",
            pr <| containsList ["Blc", "ks"] "Black",
            "[YVM, AND] in JGGGADBCOOCZYMJHZANDTOJOCZHVS",
            pr <| containsList ["YVM", "AND"] "JGGGADBCOOCZYMJHZANDTOJOCZHVS",
            "[THE, WAS] in TDGGADBCOOCTHEJHZYVMTOJOCZHVS",
            pr <| containsList ["THE", "WAS"] "TDGGADBCOOCTHEJHZYVMTOJOCZHVS",
            "[FGG, AND] in QDSGGADBCOOCZYMJHZYVMTOJOCZHVS",
            pr <| containsList ["FGG", "AND"] "QDSGGADBCOOCZYMJHZYVMTOJOCZHVS",

        "\ncandidates Function",
            "Looking for THE or AND",
            pr <| candidates ["THE", "AND"] "DGGADBCOOCZYMJHZYVMTOJOCZHVS",
            "\nLooking for THEY or WAS",
            pr <| candidates ["THEY", "WAS"] "GSDGGADBCDHZSDTOJOCZHVS",
            "\nLooking for MAN or HE",
            pr <| candidates ["MAN", "HE"] "LSDVDBCOOCZYMJHZYVMTOJOCZHVS",
            "\nLooking for NO or DAD" ,
            pr <| candidates ["NO", "DAD"] "KLGGDGDHRBTHJOCZHVS",
            "\nLooking for THEM or YES",
            pr <| candidates ["THEM", "YES"] "EMJGMVKDDKFLZSDCSFDNFDJS",


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