
let findDistanceValue (arr1: int list) (arr2: int list) (d: int) =
    let isSafe x =
        arr2 |> List.forall (fun y -> abs (x - y) > d)
    arr1 |> List.filter isSafe |> List.length


let test () =
    if findDistanceValue [4; 5; 8] [10; 9; 1; 8] 2 <> 2 then
        failwith "Test Case 1 failed"
    if findDistanceValue [1; 4; 2; 3] [-4; -3; 6; 10; 20; 30] 3 <> 2 then
        failwith "Test Case 2 failed"
    if findDistanceValue [2; 1; 100; 3] [10; 9; 8; 7] 6 <> 1 then
        failwith "Test Case 3 failed"
    if findDistanceValue [-2; -1; -3; 0] [1; 2; 3; 4] 1 <> 3 then
        failwith "Test Case 4 failed"
    if findDistanceValue [0; 0; 0; 0] [5; 6; 7; 8] 4 <> 4 then
        failwith "Test Case 5 failed"

test ()