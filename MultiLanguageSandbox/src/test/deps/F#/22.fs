
let reorderNumbers (nums: int list) (order: string) =
    let sorted = List.sort nums
    let a = sorted.[0]
    let b = sorted.[1]
    let c = sorted.[2]
    let rec processOrder (chars: char list) acc =
        match chars with
        | [] -> acc |> List.rev |> String.concat " "
        | 'A'::rest -> processOrder rest (a::acc)
        | 'B'::rest -> processOrder rest (b::acc)
        | 'C'::rest -> processOrder rest (c::acc)
        | _ -> failwith "Invalid order character"
    processOrder (List.ofSeq order) []


let check () =
    if reorderNumbers [1; 3; 5] "ABC" <> "1 3 5" then
        failwith "Test Case 1 failed"
    if reorderNumbers [6; 2; 4] "BCA" <> "4 6 2" then
        failwith "Test Case 2 failed"
    if reorderNumbers [8; 10; 9] "CAB" <> "10 8 9" then
        failwith "Test Case 3 failed"
    if reorderNumbers [15; 20; 17] "ACB" <> "15 20 17" then
        failwith "Test Case 4 failed"
    if reorderNumbers [21; 19; 23] "BAC" <> "21 19 23" then
        failwith "Test Case 5 failed"

check ()