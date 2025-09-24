
let nthTerm (a1: int) (a2: int) (n: int): int =
    let d = a2 - a1
    a1 + (n - 1) * d


let check () =
    if nthTerm 2 5 3 <> 8 then
        failwith "Test Case 1 failed"
    if nthTerm 10 13 5 <> 22 then
        failwith "Test Case 2 failed"
    if nthTerm 3 6 10 <> 30 then
        failwith "Test Case 3 failed"
    if nthTerm -5 -2 4 <> 4 then
        failwith "Test Case 4 failed"
    if nthTerm 7 10 20 <> 64 then
        failwith "Test Case 5 failed"
    if nthTerm 0 4 50 <> 196 then
        failwith "Test Case 6 failed"

check ()