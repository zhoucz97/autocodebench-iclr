
let runningSum (nums: int list) =
    let rec loop acc current nums =
        match nums with
        | [] -> List.rev acc
        | x::xs -> 
            let newCurrent = current + x
            loop (newCurrent::acc) newCurrent xs
    loop [] 0 nums


let test () =
    if runningSum [1; 2; 3; 4] <> [1; 3; 6; 10] then
        failwith "Test Case 1 failed"
    if runningSum [1; 1; 1; 1; 1] <> [1; 2; 3; 4; 5] then
        failwith "Test Case 2 failed"
    if runningSum [3; 1; 2; 10; 1] <> [3; 4; 6; 16; 17] then
        failwith "Test Case 3 failed"
    if runningSum [0; 0; 0; 0] <> [0; 0; 0; 0] then
        failwith "Test Case 4 failed"
    if runningSum [5; -1; -2; 3] <> [5; 4; 2; 5] then
        failwith "Test Case 5 failed"

test ()