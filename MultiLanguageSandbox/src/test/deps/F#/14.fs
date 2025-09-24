
let shuffleArray (nums: int list) (n: int) =
    let x = nums |> List.take n
    let y = nums |> List.skip n
    List.zip x y
    |> List.collect (fun (a, b) -> [a; b])
```

Wait, but the problem's examples show that the second parameter `n` is the number of pairs, so the total length is `2n`. For example, in the first example, the input list has 6 elements (2*3), and `n` is 3. So the function signature should be `let shuffleArray (nums: int list) (n: int) =`.

But the function above takes `n` as the number of pairs. Let me verify the examples:

First example:
Input: [2, 5, 1, 3, 4, 7], 3
- Split into x = [2, 5, 1] (first 3 elements), y = [3, 4, 7] (next 3 elements).
- Zip them into [(2,3), (5,4), (1,7)].
- Collect each pair into a list: [2;3;5;4;1;7], which matches the expected output.

Second example:
Input: [1, 2, 3, 4, 4, 3, 2, 1], 4
- Split into x = [1, 2, 3, 4], y = [4, 3, 2, 1].
- Zip into [(1,4), (2,3), (3,2), (4,1)].
- Collect into [1;4;2;3;3;2;4;1], which matches the expected output.

So the function works as intended. 

However, the problem statement mentions that the function should be `let shuffleArray (nums: int list) =`, but the examples show that it takes an additional parameter `n`. There seems to be a discrepancy here. 

If the function is supposed to take only the list and infer `n` (since the list length is 2n), then `n` would be `List.length nums / 2`. But the problem's examples include `n` as a parameter. 

Given that, the correct function signature should be `let shuffleArray (nums: int list) (n: int) =`, as shown in the examples. 

So the solution is:

```fsharp
let shuffleArray (nums: int list) (n: int) =
    let x = nums |> List.take n
    let y = nums |> List.skip n
    List.zip x y
    |> List.collect (fun (a, b) -> [a; b])


let test () =
    if shuffleArray [2; 5; 1; 3; 4; 7] <> [2; 3; 5; 4; 1; 7] then
        failwith "Test Case 1 failed"
    if shuffleArray [1; 2; 3; 4; 4; 3; 2; 1] <> [1; 4; 2; 3; 3; 2; 4; 1] then
        failwith "Test Case 2 failed"
    if shuffleArray [1; 3; 5; 7; 2; 4; 6; 8] <> [1; 2; 3; 4; 5; 6; 7; 8] then
        failwith "Test Case 3 failed"
    if shuffleArray [10; 20; 30; 40; 11; 21; 31; 41] <> [10; 11; 20; 21; 30; 31; 40; 41] then
        failwith "Test Case 4 failed"
    if shuffleArray [9; 7; 5; 3; 8; 6; 4; 2] <> [9; 8; 7; 6; 5; 4; 3; 2] then
        failwith "Test Case 5 failed"

test ()