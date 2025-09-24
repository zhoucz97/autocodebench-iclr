
let calculateAvailableData (monthlyQuota: int) (monthsUsed: int) (dataUsedPerMonth: int list) =
    let unusedData = 
        dataUsedPerMonth 
        |> List.map (fun used -> max 0 (monthlyQuota - used))
        |> List.sum
    unusedData + monthlyQuota


let test () =
    if calculateAvailableData 10 3 [4; 6; 2] <> 28 then
        failwith "Test Case 1 failed"
    if calculateAvailableData 15 2 [10; 12] <> 23 then
        failwith "Test Case 2 failed"
    if calculateAvailableData 12 4 [3; 6; 9; 4] <> 38 then
        failwith "Test Case 3 failed"
    if calculateAvailableData 10 2 [5; 5; 5] <> 15 then
        failwith "Test Case 4 failed"
    if calculateAvailableData 20 1 [15] <> 25 then
        failwith "Test Case 5 failed"
    if calculateAvailableData 8 3 [2; 4; 3] <> 23 then
        failwith "Test Case 6 failed"
    if calculateAvailableData 5 0 [] <> 5 then
        failwith "Test Case 7 failed"

test ()