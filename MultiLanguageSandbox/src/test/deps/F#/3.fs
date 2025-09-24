
let countNegatives (grid: int list list) =
    let m = grid.Length
    if m = 0 then 0
    else
        let n = grid.[0].Length
        let mutable count = 0
        let mutable row = 0
        let mutable col = n - 1
        
        while row < m && col >= 0 do
            if grid.[row].[col] < 0 then
                count <- count + (m - row)
                col <- col - 1
            else
                row <- row + 1
        count


let test () =
    if countNegatives [[4; 3; 2; -1]; [3; 2; 1; -1]; [1; 1; -1; -2]; [-1; -1; -2; -3]] <> 8 then
        failwith "Test Case 1 failed"
    if countNegatives [[3; 2]; [1; 0]] <> 0 then
        failwith "Test Case 2 failed"
    if countNegatives [[-1]] <> 1 then
        failwith "Test Case 3 failed"
    if countNegatives [[5; 4; -1]; [3; 2; -2]; [1; 0; -3]] <> 3 then
        failwith "Test Case 4 failed"
    if countNegatives [[-5; -4; -3]; [-2; -2; -1]; [-1; -1; -1]] <> 9 then
        failwith "Test Case 5 failed"

test ()