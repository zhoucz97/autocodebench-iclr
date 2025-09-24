
let findFourthVertex (coordinates: (int * int) list) : int * int =
    let xCoords = coordinates |> List.map fst
    let yCoords = coordinates |> List.map snd
    
    let x1 = xCoords.[0]
    let x2 = xCoords.[1]
    let x3 = xCoords.[2]
    
    let y1 = yCoords.[0]
    let y2 = yCoords.[1]
    let y3 = yCoords.[2]
    
    let xFourth = 
        if x1 = x2 then x3
        elif x1 = x3 then x2
        else x1
    
    let yFourth = 
        if y1 = y2 then y3
        elif y1 = y3 then y2
        else y1
    
    (xFourth, yFourth)
```

Wait, but this approach might not work for all cases. For example, if the three vertices are (5,5), (5,7), (7,5), then the x-coordinates are 5,5,7. The xFourth would be 7 (since 5 appears twice). The y-coordinates are 5,7,5. The yFourth would be 7 (since 5 appears twice). So the result is (7,7), which matches the first example.

But let's test the second example: [(10, 20); (30, 10); (10, 10)]. The x-coordinates are 10, 30, 10. So xFourth is 30. The y-coordinates are 20, 10, 10. So yFourth is 20. The result is (30, 20), which matches the second example.

So this approach seems correct.

But wait, the initial approach I described is to find the x-coordinate that appears only once among the three given vertices, and similarly for the y-coordinate. So the code should:

For xFourth:
- If x1 == x2, then xFourth is x3.
- Else if x1 == x3, then xFourth is x2.
- Else (x2 == x3), then xFourth is x1.

Similarly for yFourth.

So the code above is correct.

But let me think of another example: [(1,1), (1,2), (2,1)]. The fourth vertex should be (2,2). The x-coordinates are 1,1,2. So xFourth is 2. The y-coordinates are 1,2,1. So yFourth is 2. Correct.

Another example: [(0,0), (0,1), (1,0)]. Fourth vertex is (1,1). x-coordinates: 0,0,1 → xFourth is 1. y-coordinates: 0,1,0 → yFourth is 1. Correct.

So the approach is sound.

Thus, the function can be written as follows:

```fsharp
let findFourthVertex (coordinates: (int * int) list) : int * int =
    let x1, y1 = coordinates.[0]
    let x2, y2 = coordinates.[1]
    let x3, y3 = coordinates.[2]
    
    let xFourth = 
        if x1 = x2 then x3
        elif x1 = x3 then x2
        else x1
    
    let yFourth = 
        if y1 = y2 then y3
        elif y1 = y3 then y2
        else y1
    
    (xFourth, yFourth)
```

This should work for all cases where the rectangle has sides parallel to the axes.

### Final Answer
```fsharp
let findFourthVertex (coordinates: (int * int) list) : int * int =
    let x1, y1 = coordinates.[0]
    let x2, y2 = coordinates.[1]
    let x3, y3 = coordinates.[2]
    
    let xFourth = 
        if x1 = x2 then x3
        elif x1 = x3 then x2
        else x1
    
    let yFourth = 
        if y1 = y2 then y3
        elif y1 = y3 then y2
        else y1
    
    (xFourth, yFourth)


let check () =
    if findFourthVertex [(5, 5); (5, 7); (7, 5)] <> (7, 7) then
        failwith "Test Case 1 failed"
    if findFourthVertex [(10, 20); (30, 10); (10, 10)] <> (30, 20) then
        failwith "Test Case 2 failed"
    if findFourthVertex [(2, 2); (2, 4); (4, 2)] <> (4, 4) then
        failwith "Test Case 3 failed"
    if findFourthVertex [(3, 3); (6, 3); (3, 6)] <> (6, 6) then
        failwith "Test Case 4 failed"
    if findFourthVertex [(8, 10); (5, 10); (5, 7)] <> (8, 7) then
        failwith "Test Case 5 failed"
    if findFourthVertex [(12, 15); (9, 12); (12, 12)] <> (9, 15) then
        failwith "Test Case 6 failed"

check ()