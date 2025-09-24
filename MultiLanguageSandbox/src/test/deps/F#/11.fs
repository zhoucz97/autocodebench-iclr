
let homeworkAtQueryTime (startTime: int list) (endTime: int list) (queryTime: int) =
    List.zip startTime endTime
    |> List.filter (fun (s, e) -> queryTime >= s && queryTime <= e)
    |> List.length


// Test cases
let test () =
    if homeworkAtQueryTime [1; 2; 3] [3; 2; 7] 4 <> 1 then
        failwith "Test Case 1 failed"
    if homeworkAtQueryTime [4; 2; 2; 1] [4; 3; 7; 8] 3 <> 3 then
        failwith "Test Case 2 failed"
    if homeworkAtQueryTime [5; 1; 1; 1] [10; 3; 2; 2] 5 <> 1 then
        failwith "Test Case 3 failed"
    if homeworkAtQueryTime [1; 1; 1; 1] [4; 4; 4; 4] 4 <> 4 then
        failwith "Test Case 4 failed"
    if homeworkAtQueryTime [2; 2; 2] [3; 3; 3] 1 <> 0 then
        failwith "Test Case 5 failed"

test ()