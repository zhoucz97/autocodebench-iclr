
let mostUnhappyDay (schoolHours: int list) (extraHours: int list) : int =
    let days = [1..7] // Days of the week (1=Monday, ..., 7=Sunday)
    let combinedHours = List.zip schoolHours extraHours |> List.map (fun (s, e) -> s + e)
    
    let unhappyDays = 
        List.zip days combinedHours
        |> List.filter (fun (_, total) -> total > 8)
        |> List.map (fun (day, total) -> (day, total - 8)) // Calculate "level of unhappiness"
    
    match unhappyDays with
    | [] -> 0 // No unhappy days
    | _ -> 
        let maxUnhappiness = unhappyDays |> List.map snd |> List.max
        unhappyDays 
        |> List.filter (fun (_, u) -> u = maxUnhappiness)
        |> List.head // Take the first (earliest) day if there are ties
        |> fst


let check () =
    if mostUnhappyDay [5; 6; 7; 5; 5; 0; 0] [3; 2; 2; 3; 4; 4; 6] <> 3 then
        failwith "Test Case 1 failed"
    if mostUnhappyDay [4; 4; 4; 4; 4; 0; 0] [3; 3; 3; 3; 3; 0; 0] <> 0 then
        failwith "Test Case 2 failed"
    if mostUnhappyDay [1; 2; 3; 4; 5; 6; 7] [7; 6; 5; 4; 3; 2; 1] <> 0 then
        failwith "Test Case 3 failed"
    if mostUnhappyDay [2; 3; 4; 5; 6; 7; 1] [3; 2; 1; 1; 2; 2; 5] <> 6 then
        failwith "Test Case 4 failed"
    if mostUnhappyDay [3; 3; 3; 3; 3; 3; 3] [1; 1; 1; 1; 1; 1; 1] <> 0 then
        failwith "Test Case 5 failed"
    if mostUnhappyDay [0; 0; 0; 0; 0; 0; 9] [0; 0; 0; 0; 0; 0; 0] <> 7 then
        failwith "Test Case 6 failed"
    if mostUnhappyDay [5; 5; 5; 5; 5; 5; 5] [4; 4; 4; 4; 4; 4; 4] <> 1 then
        failwith "Test Case 7 failed"

check ()