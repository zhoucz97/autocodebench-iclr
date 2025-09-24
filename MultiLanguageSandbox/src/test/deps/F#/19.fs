
let countGoodTriplets (arr: int list) (a: int) (b: int) (c: int) =
    let n = List.length arr
    let mutable count = 0
    for i in 0 .. n - 3 do
        for j in i + 1 .. n - 2 do
            for k in j + 1 .. n - 1 do
                let x = arr.[i]
                let y = arr.[j]
                let z = arr.[k]
                if abs (x - y) <= a && abs (y - z) <= b && abs (x - z) <= c then
                    count <- count + 1
    count


let test () =
    if countGoodTriplets [3; 0; 1; 1; 9; 7] 7 2 3 <> 4 then
        failwith "Test Case 1 failed"
    if countGoodTriplets [1; 1; 2; 2; 3] 0 0 1 <> 0 then
        failwith "Test Case 2 failed"
    if countGoodTriplets [1; 2; 3; 4; 5] 1 1 1 <> 0 then
        failwith "Test Case 3 failed"

test ()