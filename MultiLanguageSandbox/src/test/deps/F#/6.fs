
let findLuckyNumbers (matrix: int list list) =
    let rows = matrix
    let cols = 
        if List.isEmpty matrix then []
        else 
            [0..(List.length (List.head matrix) - 1)] 
            |> List.map (fun col -> matrix |> List.map (fun row -> row.[col]))
    
    let rowMins = 
        rows 
        |> List.map (fun row -> 
            let minVal = List.min row
            let indices = 
                row 
                |> List.mapi (fun i x -> if x = minVal then Some i else None)
                |> List.choose id
            (minVal, indices))
    
    let colMaxs = 
        cols 
        |> List.mapi (fun colIdx col -> 
            let maxVal = List.max col
            let rowIndices = 
                col 
                |> List.mapi (fun rowIdx x -> if x = maxVal then Some rowIdx else None)
                |> List.choose id
            (maxVal, rowIndices, colIdx))
    
    let luckyNumbers = 
        rowMins 
        |> List.collect (fun (minVal, minIndices) -> 
            minIndices 
            |> List.collect (fun colIdx -> 
                colMaxs 
                |> List.choose (fun (maxVal, maxRowIndices, cIdx) -> 
                    if cIdx = colIdx && minVal = maxVal then 
                        Some minVal 
                    else 
                        None)))
        |> List.distinct
    
    luckyNumbers
```

Wait, but the above approach might not be the most efficient or correct. Let me think again. The initial approach was to find all row minima, then for each of these minima, check if they are the maximum in their column. But the code above tries to collect all possible combinations, which might not be necessary.

A better approach would be:

1. For each row, find the minimum value and its column index.
2. For each such (value, column index) pair, check if the value is the maximum in that column.
3. Collect all values that meet both conditions.

So, let's reimplement this more straightforwardly.

Here's the corrected approach:

1. For each row, find the minimum value and its column index (there could be multiple minima in a row, but any of them could be a candidate if they are the max in their column).
2. For each candidate (value, column index), check if the value is the maximum in that column.
3. Collect all such values.

So, the steps are:

- For each row, find all positions of the minimum value.
- For each such position (value, column index), check if the value is the maximum in the column.
- If yes, add the value to the result list.
- Ensure no duplicates by using distinct.

So, here's the corrected F# function:

```fsharp
let findLuckyNumbers (matrix: int list list) =
    if List.isEmpty matrix then []
    else
        let numCols = List.length (List.head matrix)
        
        // For each row, find all (value, column indices) where value is the row's min
        let rowMins =
            matrix
            |> List.mapi (fun rowIndex row ->
                let minVal = List.min row
                let minIndices =
                    row
                    |> List.mapi (fun colIndex x -> if x = minVal then Some colIndex else None)
                    |> List.choose id
                (rowIndex, minVal, minIndices))
        
        // For each column, find the max value and the row indices where it occurs
        let colMaxs =
            [0..numCols-1]
            |> List.map (fun colIndex ->
                let column =
                    matrix
                    |> List.map (fun row -> row.[colIndex])
                let maxVal = List.max column
                let maxRowIndices =
                    column
                    |> List.mapi (fun rowIndex x -> if x = maxVal then Some rowIndex else None)
                    |> List.choose id
                (colIndex, maxVal, maxRowIndices))
        
        // Now, for each (rowIndex, minVal, minColIndices) in rowMins,
        // check if any of the minColIndices has (colIndex, maxVal = minVal, and rowIndex is in maxRowIndices)
        let luckyNumbers =
            rowMins
            |> List.collect (fun (rowIndex, minVal, minColIndices) ->
                minColIndices
                |> List.choose (fun colIndex ->
                    let colInfo = colMaxs |> List.find (fun (c, _, _) -> c = colIndex)
                    let (_, maxVal, maxRowIndices) = colInfo
                    if minVal = maxVal && List.contains rowIndex maxRowIndices then
                        Some minVal
                    else
                        None))
            |> List.distinct
        
        luckyNumbers
```

Wait, but this might still have issues. For example, if a row has multiple minima in the same column, they should all be checked. But the above code should handle that.

Let me test this with the first example:

Example 1:
Matrix:
[
 [3, 7, 8],
 [9, 11, 13],
 [15, 16, 17]
]

Row mins:
- Row 0: min is 3 at column 0.
- Row 1: min is 9 at column 0.
- Row 2: min is 15 at column 0.

Column maxs:
- Column 0: max is 15 at row 2.
- Column 1: max is 16 at row 2.
- Column 2: max is 17 at row 2.

Now, check each row min:
- (0, 3, [0]): column 0's max is 15, which is not 3. So no.
- (1, 9, [0]): column 0's max is 15, not 9. No.
- (2, 15, [0]): column 0's max is 15, and row 2 is in maxRowIndices (which is [2]). So yes. Add 15.

Result: [15]. Correct.

Example 2:
Matrix:
[
 [1, 10, 4, 2],
 [9, 3, 8, 7],
 [15, 16, 17, 12]
]

Row mins:
- Row 0: min is 1 at column 0.
- Row 1: min is 3 at column 1.
- Row 2: min is 12 at column 3.

Column maxs:
- Column 0: max is 15 at row 2.
- Column 1: max is 16 at row 2.
- Column 2: max is 17 at row 2.
- Column 3: max is 12 at row 2.

Check each row min:
- (0, 1, [0]): column 0's max is 15, not 1. No.
- (1, 3, [1]): column 1's max is 16, not 3. No.
- (2, 12, [3]): column 3's max is 12, and row 2 is in maxRowIndices (which is [2]). So yes. Add 12.

Result: [12]. Correct.

So, the function seems to work for these examples.

But wait, the initial code I wrote had some issues. Let me simplify the approach:

1. For each row, find the minimum value and all its column indices.
2. For each such (value, column index), check if the value is the maximum in that column.
3. Collect all such values, ensuring no duplicates.

So, here's a more concise version:

```fsharp
let findLuckyNumbers (matrix: int list list) =
    if List.isEmpty matrix then []
    else
        let numCols = List.length (List.head matrix)
        
        // For each row, find all (value, column indices) where value is the row's min
        let rowMins =
            matrix
            |> List.mapi (fun rowIndex row ->
                let minVal = List.min row
                let minIndices =
                    row
                    |> List.mapi (fun colIndex x -> if x = minVal then Some colIndex else None)
                    |> List.choose id
                (rowIndex, minVal, minIndices))
        
        // For each column, find the max value
        let colMaxs =
            [0..numCols-1]
            |> List.map (fun colIndex ->
                let column =
                    matrix
                    |> List.map (fun row -> row.[colIndex])
                let maxVal = List.max column
                (colIndex, maxVal))
        
        // Now, for each (rowIndex, minVal, minColIndices) in rowMins,
        // check if any of the minColIndices has (colIndex, maxVal = minVal)
        let luckyNumbers =
            rowMins
            |> List.collect (fun (rowIndex, minVal, minColIndices) ->
                minColIndices
                |> List.choose (fun colIndex ->
                    let (_, maxVal) = colMaxs.[colIndex]
                    if minVal = maxVal then Some minVal else None))
            |> List.distinct
        
        luckyNumbers
```

This should work. Let me test it again.

Example 1:
Row mins:
- (0, 3, [0])
- (1, 9, [0])
- (2, 15, [0])

Col maxs:
- (0, 15)
- (1, 16)
- (2, 17)

Check:
- (0, 3, [0]): col 0's max is 15 != 3. No.
- (1, 9, [0]): col 0's max is 15 != 9. No.
- (2, 15, [0]): col 0's max is 15 == 15. Yes. Add 15.

Result: [15]. Correct.

Example 2:
Row mins:
- (0, 1, [0])
- (1, 3, [1])
- (2, 12, [3])

Col maxs:
- (0, 15)
- (1, 16)
- (2, 17)
- (3, 12)

Check:
- (0, 1, [0]): col 0's max is 15 != 1. No.
- (1, 3, [1]): col 1's max is 16 != 3. No.
- (2, 12, [3]): col 3's max is 12 == 12. Yes. Add 12.

Result: [12]. Correct.

So, this version should work. The key is to:

1. For each row, find all positions of the minimum value.
2. For each such position, check if the value is the maximum in its column.
3. Collect all such values, ensuring no duplicates.

Thus, the final F# function is:

```fsharp
let findLuckyNumbers (matrix: int list list) =
    if List.isEmpty matrix then []
    else
        let numCols = List.length (List.head matrix)
        
        // For each row, find all (value, column indices) where value is the row's min
        let rowMins =
            matrix
            |> List.mapi (fun rowIndex row ->
                let minVal = List.min row
                let minIndices =
                    row
                    |> List.mapi (fun colIndex x -> if x = minVal then Some colIndex else None)
                    |> List.choose id
                (rowIndex, minVal, minIndices))
        
        // For each column, find the max value
        let colMaxs =
            [0..numCols-1]
            |> List.map (fun colIndex ->
                let column =
                    matrix
                    |> List.map (fun row -> row.[colIndex])
                let maxVal = List.max column
                (colIndex, maxVal))
        
        // Now, for each (rowIndex, minVal, minColIndices) in rowMins,
        // check if any of the minColIndices has (colIndex, maxVal = minVal)
        let luckyNumbers =
            rowMins
            |> List.collect (fun (_, minVal, minColIndices) ->
                minColIndices
                |> List.choose (fun colIndex ->
                    let (_, maxVal) = colMaxs.[colIndex]
                    if minVal = maxVal then Some minVal else None))
            |> List.distinct
        
        luckyNumbers


let test () =
    if findLuckyNumbers [[3; 7; 8]; [9; 11; 13]; [15; 16; 17]] <> [15] then
        failwith "Test Case 1 failed"
    if findLuckyNumbers [[1; 10; 4; 2]; [9; 3; 8; 7]; [15; 16; 17; 12]] <> [12] then
        failwith "Test Case 2 failed"
    if findLuckyNumbers [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]] <> [7] then
        failwith "Test Case 3 failed"
    if findLuckyNumbers [[7; 8]; [1; 6]] <> [7] then
        failwith "Test Case 4 failed"
    if findLuckyNumbers [[11; 17; 4; 3]; [9; 5; 8; 7]; [15; 2; 16; 12]] <> [] then
        failwith "Test Case 5 failed"

test ()