
let maxProduct (nums: int list) =
    let sorted = List.sortDescending nums
    let a = sorted.[0] - 1
    let b = sorted.[1] - 1
    a * b


let test () =
    if maxProduct [3; 4; 5; 2] <> 12 then
        failwith "Test Case 1 failed"
    if maxProduct [1; 5; 4; 5] <> 16 then
        failwith "Test Case 2 failed"
    if maxProduct [1; 2; 3] <> 2 then
        failwith "Test Case 3 failed"
    if maxProduct [10; 2; 5; 2] <> 36 then
        failwith "Test Case 4 failed"
    if maxProduct [1; 1] <> 0 then
        failwith "Test Case 5 failed"

test ()