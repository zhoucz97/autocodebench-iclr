
let existsDouble (arr: int list) =
    let set = Set.ofList arr
    arr |> List.exists (fun x -> 
        let double = 2 * x
        Set.contains double set)


let test () =
    if existsDouble [1; 2; 3; 4] <> true then
        failwith "Test Case 1 failed"
    if existsDouble [1; 3; 5; 7] <> false then
        failwith "Test Case 2 failed"
    if existsDouble [10; 5; 3; 2] <> true then
        failwith "Test Case 3 failed"
    if existsDouble [6; 7; 9; 12] <> true then
        failwith "Test Case 4 failed"
    if existsDouble [20; 5; 10; 15] <> true then
        failwith "Test Case 5 failed"
    if existsDouble [4; 2; 1; 8] <> true then
        failwith "Test Case 6 failed"
    if existsDouble [11; 22; 33; 44] <> true then
        failwith "Test Case 7 failed"

test ()