
let hanoiTowerSteps (n: int) =
    if n <= 0 then
        0
    else
        pown 2 n - 1

// Test cases
printfn "%d" (hanoiTowerSteps 1)  // Output: 1
printfn "%d" (hanoiTowerSteps 3)  // Output: 7
printfn "%d" (hanoiTowerSteps 4)  // Output: 15


let checkHanoi () =
    if hanoiTowerSteps 1 <> 1 then
        failwith "Test Case 1 failed"
    if hanoiTowerSteps 3 <> 7 then
        failwith "Test Case 2 failed"
    if hanoiTowerSteps 4 <> 15 then
        failwith "Test Case 3 failed"
    if hanoiTowerSteps 5 <> 31 then
        failwith "Test Case 4 failed"
    if hanoiTowerSteps 6 <> 63 then
        failwith "Test Case 5 failed"
    if hanoiTowerSteps 10 <> 1023 then
        failwith "Test Case 6 failed"
    if hanoiTowerSteps 0 <> 0 then
        failwith "Test Case 7 failed"

checkHanoi ()