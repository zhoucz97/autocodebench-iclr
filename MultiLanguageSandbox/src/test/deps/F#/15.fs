
let finalPrices (prices: int list) =
    let rec findDiscountIndex i =
        if i >= List.length prices then -1
        else
            let currentPrice = List.item i prices
            let rec checkNext j =
                if j >= List.length prices then -1
                else
                    let nextPrice = List.item j prices
                    if nextPrice <= currentPrice then j
                    else checkNext (j + 1)
            checkNext (i + 1)
    
    let rec processList i acc =
        if i >= List.length prices then List.rev acc
        else
            let discountIndex = findDiscountIndex i
            let discount = if discountIndex <> -1 then List.item discountIndex prices else 0
            let finalPrice = List.item i prices - discount
            processList (i + 1) (finalPrice :: acc)
    
    processList 0 []
```

Wait, but this approach might not be the most efficient for large lists because for each element, it scans the remaining elements linearly. However, given the constraints (the problem doesn't specify any constraints, but typically for such problems, the list might be small), this approach should work.

But let's think of a more efficient way. We can use a stack to keep track of indices of prices for which we haven't found a discount yet. This approach is O(n) time complexity.

Here's the optimized approach:

1. **Initialize a stack**: The stack will store indices of prices for which we haven't applied a discount yet.
2. **Iterate through each price**: For each price at index `i`, while the stack is not empty and the current price is less than or equal to the price at the index stored at the top of the stack, apply the discount to the price at the top of the stack.
3. **Push the current index onto the stack**: This ensures that we process it later if a suitable discount is found.
4. **Construct the result list**: After processing all elements, any remaining indices in the stack have no discount, so their final price remains the same.

### Optimized Solution Code
```fsharp
let finalPrices (prices: int list) =
    let n = List.length prices
    let mutable stack = []
    let mutable result = List.map (fun x -> x) prices // Initialize result with original prices
    
    for i in 0 .. n - 1 do
        while not (List.isEmpty stack) && (List.item i prices) <= (List.item (List.head stack) prices) do
            let j = List.head stack
            stack <- List.tail stack
            result <- List.mapi (fun idx x -> if idx = j then x - (List.item i prices) else x) result
        stack <- i :: stack
    
    result
```

Wait, but this might not be the most efficient in F# due to the immutable nature of lists. Let me think again. Maybe using an array would be better for mutability.

Here's another approach using a mutable array for the result:

```fsharp
let finalPrices (prices: int list) =
    let n = List.length prices
    let pricesArray = List.toArray prices
    let result = Array.copy pricesArray
    let stack = System.Collections.Generic.Stack<int>()
    
    for i in 0 .. n - 1 do
        while stack.Count > 0 && pricesArray.[i] <= pricesArray.[stack.Peek()] do
            let j = stack.Pop()
            result.[j] <- result.[j] - pricesArray.[i]
        stack.Push(i)
    
    Array.toList result
```

This should be more efficient. Let's test it with the examples.

For the first example:
Input: [8, 4, 6, 2, 3]
Processing:
- i=0 (8): stack is empty, push 0.
- i=1 (4): 4 <= 8 (top of stack is 0), so result[0] = 8 - 4 = 4. Pop 0. Stack is empty, push 1.
- i=2 (6): stack has 1. 6 <= 4? No. Push 2.
- i=3 (2): stack has 2. 2 <= 6? Yes. result[2] = 6 - 2 = 4. Pop 2. Stack has 1. 2 <= 4? Yes. result[1] = 4 - 2 = 2. Pop 1. Stack is empty. Push 3.
- i=4 (3): stack has 3. 3 <= 2? No. Push 4.
Final result: [4, 2, 4, 2, 3] — correct.

Second example:
Input: [1, 2, 3, 4, 5]
Processing:
- i=0 (1): push 0.
- i=1 (2): 2 <= 1? No. Push 1.
- i=2 (3): 3 <= 2? No. Push 2.
- i=3 (4): 4 <= 3? No. Push 3.
- i=4 (5): 5 <= 4? No. Push 4.
No discounts applied. Result is [1, 2, 3, 4, 5] — correct.

So this approach works.

### Final Answer
```fsharp
let finalPrices (prices: int list) =
    let n = List.length prices
    let pricesArray = List.toArray prices
    let result = Array.copy pricesArray
    let stack = System.Collections.Generic.Stack<int>()
    
    for i in 0 .. n - 1 do
        while stack.Count > 0 && pricesArray.[i] <= pricesArray.[stack.Peek()] do
            let j = stack.Pop()
            result.[j] <- result.[j] - pricesArray.[i]
        stack.Push(i)
    
    Array.toList result


let test () =
    if finalPrices [8; 4; 6; 2; 3] <> [4; 2; 4; 2; 3] then
        failwith "Test Case 1 failed"
    if finalPrices [1; 2; 3; 4; 5] <> [1; 2; 3; 4; 5] then
        failwith "Test Case 2 failed"
    if finalPrices [10; 1; 1; 6] <> [9; 0; 1; 6] then
        failwith "Test Case 3 failed"
    if finalPrices [4; 2; 4; 2; 3] <> [2; 0; 2; 2; 3] then
        failwith "Test Case 4 failed"
    if finalPrices [7; 7; 7; 7; 7] <> [0; 0; 0; 0; 7] then
        failwith "Test Case 5 failed"

test ()