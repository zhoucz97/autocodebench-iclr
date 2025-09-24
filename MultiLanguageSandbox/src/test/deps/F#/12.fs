
let canMakeSameAsTarget (target: int list) (arr: int list) =
    let rec sortList lst =
        match lst with
        | [] -> []
        | x::xs -> insert x (sortList xs)
    and insert x lst =
        match lst with
        | [] -> [x]
        | y::ys when x <= y -> x::y::ys
        | y::ys -> y::(insert x ys)
    
    let sortedTarget = sortList target
    let sortedArr = sortList arr
    sortedTarget = sortedArr
```

Wait, but the above approach uses insertion sort, which might not be the most efficient, but for small lists, it's acceptable. However, in F#, we can use built-in functions to sort and compare lists more efficiently.

Here's a more efficient version using built-in sorting:

```fsharp
let canMakeSameAsTarget (target: int list) (arr: int list) =
    List.sort target = List.sort arr


let test () =
    if canMakeSameAsTarget [1; 2; 3; 4] [2; 4; 1; 3] <> true then
        failwith "Test Case 1 failed"
    if canMakeSameAsTarget [7; 7; 7] [7; 7; 7] <> true then
        failwith "Test Case 2 failed"
    if canMakeSameAsTarget [5; 1; 3] [1; 3; 5] <> true then
        failwith "Test Case 3 failed"
    if canMakeSameAsTarget [1; 4; 3; 2] [2; 3; 4; 1] <> true then
        failwith "Test Case 4 failed"
    if canMakeSameAsTarget [1; 2; 1; 2] [2; 1; 2; 1] <> true then
        failwith "Test Case 5 failed"

test ()