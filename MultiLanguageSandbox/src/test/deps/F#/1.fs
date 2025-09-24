
let hasCloseElements (numbers: float list) (threshold: float) =
    let rec checkPairs remaining seen =
        match remaining with
        | [] -> false
        | x::xs ->
            let isCloseToAny = List.exists (fun y -> abs (x - y) < threshold) seen
            if isCloseToAny then true
            else checkPairs xs (x::seen)
    checkPairs numbers []


// Test cases
let check () =
    if hasCloseElements [1.0; 2.0; 3.9; 4.0; 5.0; 2.2] 0.3 <> true then
        failwith "Test Case 1 failed"
    if hasCloseElements [1.0; 2.0; 3.9; 4.0; 5.0; 2.2] 0.05 <> false then
        failwith "Test Case 2 failed"
    if hasCloseElements [1.0; 2.0; 5.9; 4.0; 5.0] 0.95 <> true then
        failwith "Test Case 3 failed"
    if hasCloseElements [1.0; 2.0; 5.9; 4.0; 5.0] 0.8 <> false then
        failwith "Test Case 4 failed"
    if hasCloseElements [1.0; 2.0; 3.0; 4.0; 5.0; 2.0] 0.1 <> true then
        failwith "Test Case 5 failed"
    if hasCloseElements [1.1; 2.2; 3.1; 4.1; 5.1] 1.0 <> true then
        failwith "Test Case 6 failed"
    if hasCloseElements [1.1; 2.2; 3.1; 4.1; 5.1] 0.5 <> false then
        failwith "Test Case 7 failed"

check ()