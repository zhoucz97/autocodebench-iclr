
let smallerNumbersThanCurrent (nums: int list) =
    let sorted = List.sort nums
    nums |> List.map (fun x -> 
        let index = List.findIndex (fun y -> y = x) sorted
        index)
```

However, this solution has a time complexity of O(n^2) because for each element, it scans the sorted list to find its position. For better performance, especially with larger lists, we can optimize it:

```fsharp
let smallerNumbersThanCurrent (nums: int list) =
    let sorted = List.sort nums
    let counts = 
        sorted 
        |> List.mapi (fun i x -> (x, i))
        |> List.groupBy fst
        |> List.map (fun (k, vs) -> (k, List.head vs)) // take the first occurrence for duplicates
        |> Map.ofList
    nums |> List.map (fun x -> counts.[x])
```

But the simplest correct solution that passes the given test cases is:

```fsharp
let smallerNumbersThanCurrent (nums: int list) =
    nums |> List.map (fun x -> 
        nums |> List.filter (fun y -> y < x) |> List.length)


let test () =
    if smallerNumbersThanCurrent [8; 1; 2; 2; 3] <> [4; 0; 1; 1; 3] then
        failwith "Test Case 1 failed"
    if smallerNumbersThanCurrent [6; 5; 4; 8] <> [2; 1; 0; 3] then
        failwith "Test Case 2 failed"
    if smallerNumbersThanCurrent [7; 7; 7; 7] <> [0; 0; 0; 0] then
        failwith "Test Case 3 failed"
    if smallerNumbersThanCurrent [1; 2; 3; 4; 5] <> [0; 1; 2; 3; 4] then
        failwith "Test Case 4 failed"
    if smallerNumbersThanCurrent [5; 4; 3; 2; 1] <> [4; 3; 2; 1; 0] then
        failwith "Test Case 5 failed"

test ()