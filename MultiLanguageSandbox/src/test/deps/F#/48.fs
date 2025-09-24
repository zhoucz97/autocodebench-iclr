
let powerOfTwo (n: int): int =
    if n < 0 || n > 30 then
        failwith "n must be between 0 and 30 inclusive"
    else
        1 <<< n


let check () =
    if powerOfTwo 4 <> 16 then
        failwith "Test Case 1 failed"
    if powerOfTwo 5 <> 32 then
        failwith "Test Case 2 failed"
    if powerOfTwo 0 <> 1 then
        failwith "Test Case 3 failed"
    if powerOfTwo 10 <> 1024 then
        failwith "Test Case 4 failed"
    if powerOfTwo 15 <> 32768 then
        failwith "Test Case 5 failed"
    if powerOfTwo 20 <> 1048576 then
        failwith "Test Case 6 failed"

check ()