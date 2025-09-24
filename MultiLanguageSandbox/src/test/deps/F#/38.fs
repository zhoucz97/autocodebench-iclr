
let countWordClasses (words: string list) =
    let normalize (word: string) =
        word.ToCharArray()
        |> Array.sort
        |> System.String
    words
    |> List.map normalize
    |> Set.ofList
    |> Set.count


let testCountWordClasses () =
    if countWordClasses ["ABAC"; "BAAC"; "XYZ"; "ZYX"; "ABC"] <> 3 then
        failwith "Test Case 1 failed"
    if countWordClasses ["AAAA"; "AAAB"; "BAAA"] <> 2 then
        failwith "Test Case 2 failed"
    if countWordClasses ["APPLE"; "PPALE"; "BYTE"; "BETY"] <> 2 then
        failwith "Test Case 3 failed"
    if countWordClasses [] <> 0 then
        failwith "Test Case 4 failed"
    if countWordClasses ["SINGLE"] <> 1 then
        failwith "Test Case 5 failed"

testCountWordClasses ()