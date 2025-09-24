
let calculateExpression (a: int) (b: int) (c: int): int =
    (a + b) * c



let check () =
    if calculateExpression 1 2 3 <> 9 then
        failwith "Test Case 1 failed"
    if calculateExpression -1 2 -3 <> -3 then
        failwith "Test Case 2 failed"
    if calculateExpression 0 0 1 <> 0 then
        failwith "Test Case 3 failed"
    if calculateExpression 100 -50 2 <> 100 then
        failwith "Test Case 4 failed"
    if calculateExpression -100 50 -2 <> 100 then
        failwith "Test Case 5 failed"

check ()