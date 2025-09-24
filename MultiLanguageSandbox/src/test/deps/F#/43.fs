
let asciiToChar (asciiCode: int): char =
    if asciiCode >= 0 && asciiCode < 128 then
        char asciiCode
    else
        failwith "ASCII code must be a positive integer less than 128"


let check () =
    if asciiToChar 66 <> 'B' then
        failwith "Test Case 1 failed"
    if asciiToChar 35 <> '#' then
        failwith "Test Case 2 failed"
    if asciiToChar 48 <> '0' then
        failwith "Test Case 3 failed"
    if asciiToChar 57 <> '9' then
        failwith "Test Case 4 failed"
    if asciiToChar 97 <> 'a' then
        failwith "Test Case 5 failed"
    if asciiToChar 122 <> 'z' then
        failwith "Test Case 6 failed"

check ()