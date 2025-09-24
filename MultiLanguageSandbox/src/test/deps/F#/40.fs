
let maxPens (a: int) (b: int): int =
    let totalJiao = a * 10 + b
    totalJiao / 19


let check () =
    if maxPens 20 5 <> 10 then
        failwith "Test Case 1 failed"
    if maxPens 0 9 <> 0 then
        failwith "Test Case 2 failed"
    if maxPens 3 1 <> 1 then
        failwith "Test Case 3 failed"
    if maxPens 57 0 <> 30 then
        failwith "Test Case 4 failed"

check ()