
let hasThreeConsecutiveOdds (arr: int list) =
    let rec checkConsecutive oddsLeft lst =
        match lst with
        | [] -> false
        | x::xs ->
            if x % 2 <> 0 then
                if oddsLeft = 1 then true
                else checkConsecutive (oddsLeft - 1) xs
            else
                checkConsecutive 3 xs
    checkConsecutive 3 arr


let test () =
    if hasThreeConsecutiveOdds [2; 6; 4; 1] <> false then
        failwith "Test Case 1 failed"
    if hasThreeConsecutiveOdds [1; 2; 34; 3; 4; 5; 7; 23; 12] <> true then
        failwith "Test Case 2 failed"
    if hasThreeConsecutiveOdds [1; 3; 5; 7; 9] <> true then
        failwith "Test Case 3 failed"
    if hasThreeConsecutiveOdds [2; 4; 6; 8; 10] <> false then
        failwith "Test Case 4 failed"
    if hasThreeConsecutiveOdds [1; 1; 2; 3; 5] <> false then
        failwith "Test Case 5 failed"

test ()