
let intBoolIntConversion (intValue: int): int =
    let boolValue = intValue <> 0
    if boolValue then 1 else 0


let check () =
    if intBoolIntConversion 5 <> 1 then
        failwith "Test Case 1 failed"
    if intBoolIntConversion 0 <> 0 then
        failwith "Test Case 2 failed"
    if intBoolIntConversion -1 <> 1 then
        failwith "Test Case 3 failed"
    if intBoolIntConversion 100 <> 1 then
        failwith "Test Case 4 failed"
    if intBoolIntConversion 127 <> 1 then
        failwith "Test Case 5 failed"
    if intBoolIntConversion -127 <> 1 then
        failwith "Test Case 6 failed"

check ()