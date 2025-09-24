
let kidsWithCandies (candies: int list) (extraCandies: int) =
    let maxCandies = List.max candies
    candies |> List.map (fun x -> x + extraCandies >= maxCandies)


let test () =
    if kidsWithCandies [2; 3; 5; 1; 3] 3 <> [true; true; true; false; true] then
        failwith "Test Case 1 failed"
    if kidsWithCandies [4; 2; 1; 1; 2] 1 <> [true; false; false; false; false] then
        failwith "Test Case 2 failed"
    if kidsWithCandies [1; 1; 1; 1; 1] 5 <> [true; true; true; true; true] then
        failwith "Test Case 3 failed"
    if kidsWithCandies [5; 10; 5; 10; 10] 5 <> [true; true; true; true; true] then
        failwith "Test Case 4 failed"
    if kidsWithCandies [10; 5; 3; 2; 1] 2 <> [true; false; false; false; false] then
        failwith "Test Case 5 failed"

test ()