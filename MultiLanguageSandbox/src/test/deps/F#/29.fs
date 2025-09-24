
let canMatchFit (n: int) (w: int) (h: int) (matches: int list) =
    let diagonal = sqrt(float (w * w + h * h))
    matches
    |> List.map (fun matchLength ->
        if float matchLength <= diagonal then "DA" else "NE")


let check () =
    let test1 = canMatchFit 3 3 4 [2; 5; 6]
    let expected1 = ["DA"; "DA"; "NE"]
    if test1 <> expected1 then
        failwith "Test Case 1 failed"

    let test2 = canMatchFit 2 5 5 [7; 3]
    let expected2 = ["DA"; "DA"]
    if test2 <> expected2 then
        failwith "Test Case 2 failed"

    let test3 = canMatchFit 4 10 10 [5; 12; 8; 15]
    let expected3 = ["DA"; "DA"; "DA"; "NE"]
    if test3 <> expected3 then
        failwith "Test Case 3 failed"

    let test4 = canMatchFit 3 6 8 [10; 6; 7]
    let expected4 = ["DA"; "DA"; "DA"]
    if test4 <> expected4 then
        failwith "Test Case 4 failed"

    printfn "All tests passed!"

check ()