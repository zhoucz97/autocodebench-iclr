
let createTargetArray (nums: int list) (index: int list) =
    let rec insertIntoList lst idx value =
        match idx with
        | 0 -> value :: lst
        | _ -> 
            match lst with
            | [] -> [value]
            | head::tail -> head :: (insertIntoList tail (idx - 1) value)
    
    let rec buildTarget acc nums indices =
        match nums, indices with
        | [], [] -> List.rev acc
        | num::restNums, idx::restIndices -> 
            let newAcc = insertIntoList acc idx num
            buildTarget newAcc restNums restIndices
        | _ -> failwith "Mismatched lengths"
    
    buildTarget [] nums index
```

Wait, but the above approach might not work correctly because inserting into a list in F# is O(n) for each insertion, and doing this for each element could lead to O(n^2) time complexity, which might not be efficient for large lists. However, given the constraints (the examples are small), this should work.

But let's think again. The initial approach is to start with an empty list and for each element in nums, insert it into the current target list at the specified index. For example:

For nums = [0, 1, 2, 3, 4] and index = [0, 1, 2, 2, 1]:

- Start with []
- Insert 0 at 0: [0]
- Insert 1 at 1: [0, 1]
- Insert 2 at 2: [0, 1, 2]
- Insert 3 at 2: [0, 1, 3, 2]
- Insert 4 at 1: [0, 4, 1, 3, 2]

Which matches the first example.

So the function should work as follows:

1. Initialize an empty list.
2. For each element in nums and corresponding index in index:
   - Insert the element into the current list at the specified index.
   - The insertion should shift existing elements to the right if necessary.

The insertIntoList function should handle this. For example, inserting value 4 at index 1 into [0, 1, 3, 2] would produce [0, 4, 1, 3, 2].

So the code should work.

Let me test the first example:

createTargetArray [0;1;2;3;4] [0;1;2;2;1]

Initial acc is []

First iteration: num=0, idx=0. insertIntoList [] 0 0 → [0]. New acc is [0].

Second iteration: num=1, idx=1. insertIntoList [0] 1 1 → 0 :: (insertIntoList [] 0 1) → 0 :: [1] → [0,1]. New acc is [0,1].

Third iteration: num=2, idx=2. insertIntoList [0,1] 2 2 → 0 :: (insertIntoList [1] 1 2) → 0 :: (1 :: (insertIntoList [] 0 2)) → 0 :: (1 :: [2]) → [0,1,2]. New acc is [0,1,2].

Fourth iteration: num=3, idx=2. insertIntoList [0,1,2] 2 3 → 0 :: (insertIntoList [1,2] 1 3) → 0 :: (1 :: (insertIntoList [2] 0 3)) → 0 :: (1 :: (3 :: [2])) → [0,1,3,2]. New acc is [0,1,3,2].

Fifth iteration: num=4, idx=1. insertIntoList [0,1,3,2] 1 4 → 0 :: (insertIntoList [1,3,2] 0 4) → 0 :: (4 :: [1,3,2]) → [0,4,1,3,2]. New acc is [0,4,1,3,2].

Then buildTarget returns List.rev [0,4,1,3,2] → [0,4,1,3,2], which matches the first example.

Similarly, the second example:

createTargetArray [1;2;3;4;0] [0;1;2;3;0]

Initial acc is []

First iteration: num=1, idx=0. insertIntoList [] 0 1 → [1]. New acc is [1].

Second iteration: num=2, idx=1. insertIntoList [1] 1 2 → 1 :: (insertIntoList [] 0 2) → 1 :: [2] → [1,2]. New acc is [1,2].

Third iteration: num=3, idx=2. insertIntoList [1,2] 2 3 → 1 :: (insertIntoList [2] 1 3) → 1 :: (2 :: (insertIntoList [] 0 3)) → 1 :: (2 :: [3]) → [1,2,3]. New acc is [1,2,3].

Fourth iteration: num=4, idx=3. insertIntoList [1,2,3] 3 4 → 1 :: (insertIntoList [2,3] 2 4) → 1 :: (2 :: (insertIntoList [3] 1 4)) → 1 :: (2 :: (3 :: (insertIntoList [] 0 4))) → 1 :: (2 :: (3 :: [4])) → [1,2,3,4]. New acc is [1,2,3,4].

Fifth iteration: num=0, idx=0. insertIntoList [1,2,3,4] 0 0 → 0 :: [1,2,3,4] → [0,1,2,3,4]. New acc is [0,1,2,3,4].

Then buildTarget returns List.rev [0,1,2,3,4] → [0,1,2,3,4], which matches the second example.

So the function works.

But wait, the function returns the reversed list at the end. But in the examples, the output is not reversed. For instance, the first example's output is [0,4,1,3,2], which is the same as the accumulated list before reversing. So the List.rev is unnecessary. The accumulated list is built in reverse order during the recursion, but since we're inserting at the front (using ::), the order is preserved when we build the list.

Wait, no. Let me re-examine the process.

In the first example, the accumulated list after each step is:

Initial: []

After first insertion (0 at 0): [0]

After second insertion (1 at 1): [0,1]

After third insertion (2 at 2): [0,1,2]

After fourth insertion (3 at 2): [0,1,3,2]

After fifth insertion (4 at 1): [0,4,1,3,2]

So the accumulated list is [0,4,1,3,2], which is the correct output. So the List.rev is not needed. The function should return the accumulated list directly.

So the corrected function is:

```fsharp
let createTargetArray (nums: int list) (index: int list) =
    let rec insertIntoList lst idx value =
        match idx with
        | 0 -> value :: lst
        | _ -> 
            match lst with
            | [] -> [value]
            | head::tail -> head :: (insertIntoList tail (idx - 1) value)
    
    let rec buildTarget acc nums indices =
        match nums, indices with
        | [], [] -> acc
        | num::restNums, idx::restIndices -> 
            let newAcc = insertIntoList acc idx num
            buildTarget newAcc restNums restIndices
        | _ -> failwith "Mismatched lengths"
    
    buildTarget [] nums index
```

Now, testing the first example:

createTargetArray [0;1;2;3;4] [0;1;2;2;1]

buildTarget starts with acc = [], nums = [0;1;2;3;4], indices = [0;1;2;2;1].

First call: num=0, idx=0. insertIntoList [] 0 0 → [0]. newAcc = [0]. Recurse with acc=[0], nums=[1;2;3;4], indices=[1;2;2;1].

Second call: num=1, idx=1. insertIntoList [0] 1 1 → 0 :: (insertIntoList [] 0 1) → 0 :: [1] → [0,1]. newAcc = [0,1]. Recurse with acc=[0,1], nums=[2;3;4], indices=[2;2;1].

Third call: num=2, idx=2. insertIntoList [0,1] 2 2 → 0 :: (insertIntoList [1] 1 2) → 0 :: (1 :: (insertIntoList [] 0 2)) → 0 :: (1 :: [2]) → [0,1,2]. newAcc = [0,1,2]. Recurse with acc=[0,1,2], nums=[3;4], indices=[2;1].

Fourth call: num=3, idx=2. insertIntoList [0,1,2] 2 3 → 0 :: (insertIntoList [1,2] 1 3) → 0 :: (1 :: (insertIntoList [2] 0 3)) → 0 :: (1 :: (3 :: [2])) → [0,1,3,2]. newAcc = [0,1,3,2]. Recurse with acc=[0,1,3,2], nums=[4], indices=[1].

Fifth call: num=4, idx=1. insertIntoList [0,1,3,2] 1 4 → 0 :: (insertIntoList [1,3,2] 0 4) → 0 :: (4 :: [1,3,2]) → [0,4,1,3,2]. newAcc = [0,4,1,3,2]. Recurse with acc=[0,4,1,3,2], nums=[], indices=[].

Base case: return acc = [0,4,1,3,2], which matches the first example.

Second example:

createTargetArray [1;2;3;4;0] [0;1;2;3;0]

buildTarget starts with acc = [], nums = [1;2;3;4;0], indices = [0;1;2;3;0].

First call: num=1, idx=0. insertIntoList [] 0 1 → [1]. newAcc = [1]. Recurse with acc=[1], nums=[2;3;4;0], indices=[1;2;3;0].

Second call: num=2, idx=1. insertIntoList [1] 1 2 → 1 :: (insertIntoList [] 0 2) → 1 :: [2] → [1,2]. newAcc = [1,2]. Recurse with acc=[1,2], nums=[3;4;0], indices=[2;3;0].

Third call: num=3, idx=2. insertIntoList [1,2] 2 3 → 1 :: (insertIntoList [2] 1 3) → 1 :: (2 :: (insertIntoList [] 0 3)) → 1 :: (2 :: [3]) → [1,2,3]. newAcc = [1,2,3]. Recurse with acc=[1,2,3], nums=[4;0], indices=[3;0].

Fourth call: num=4, idx=3. insertIntoList [1,2,3] 3 4 → 1 :: (insertIntoList [2,3] 2 4) → 1 :: (2 :: (insertIntoList [3] 1 4)) → 1 :: (2 :: (3 :: (insertIntoList [] 0 4))) → 1 :: (2 :: (3 :: [4])) → [1,2,3,4]. newAcc = [1,2,3,4]. Recurse with acc=[1,2,3,4], nums=[0], indices=[0].

Fifth call: num=0, idx=0. insertIntoList [1,2,3,4] 0 0 → 0 :: [1,2,3,4] → [0,1,2,3,4]. newAcc = [0,1,2,3,4]. Recurse with acc=[0,1,2,3,4], nums=[], indices=[].

Base case: return acc = [0,1,2,3,4], which matches the second example.

So the function works correctly without the List.rev.

Thus, the correct F# function is:

```fsharp
let createTargetArray (nums: int list) (index: int list) =
    let rec insertIntoList lst idx value =
        match idx with
        | 0 -> value :: lst
        | _ -> 
            match lst with
            | [] -> [value]
            | head::tail -> head :: (insertIntoList tail (idx - 1) value)
    
    let rec buildTarget acc nums indices =
        match nums, indices with
        | [], [] -> acc
        | num::restNums, idx::restIndices -> 
            let newAcc = insertIntoList acc idx num
            buildTarget newAcc restNums restIndices
        | _ -> failwith "Mismatched lengths"
    
    buildTarget [] nums index


let test () =
    if createTargetArray [0; 1; 2; 3; 4] [0; 1; 2; 2; 1] <> [0; 4; 1; 3; 2] then
        failwith "Test Case 1 failed"
    if createTargetArray [1; 2; 3; 4; 0] [0; 1; 2; 3; 0] <> [0; 1; 2; 3; 4] then
        failwith "Test Case 2 failed"
    if createTargetArray [4; 3; 2; 1; 0] [0; 0; 0; 0; 0] <> [0; 1; 2; 3; 4] then
        failwith "Test Case 3 failed"

test ()