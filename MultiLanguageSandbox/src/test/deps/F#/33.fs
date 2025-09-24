
let calculateFinalScore (scores: int list) =
    if List.length scores <= 2 then
        0.0 // or handle this case differently if needed
    else
        let sorted = List.sort scores
        let trimmed = sorted.[1..List.length sorted - 2]
        let sum = List.sum trimmed
        float sum / float (List.length trimmed)


let testCalculateFinalScore () =
    if calculateFinalScore [9; 5; 6; 8; 9] <> 7.67 then
        failwith "Test Case 1 failed"
    if calculateFinalScore [10; 10; 10; 10; 10] <> 10.00 then
        failwith "Test Case 2 failed"
    if calculateFinalScore [0; 0; 0; 0; 0] <> 0.00 then
        failwith "Test Case 3 failed"
    if calculateFinalScore [5; 6; 7; 8; 9] <> 7.00 then
        failwith "Test Case 4 failed"
    if calculateFinalScore [1; 2; 3; 4; 5] <> 3.00 then
        failwith "Test Case 5 failed"

testCalculateFinalScore ()