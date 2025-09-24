
let getAsciiCode (character: char): int =
    int character


let check () =
    if getAsciiCode 'B' <> 66 then
        failwith "Test Case 1 failed"
    if getAsciiCode '#' <> 35 then
        failwith "Test Case 2 failed"
    if getAsciiCode '0' <> 48 then
        failwith "Test Case 3 failed"
    if getAsciiCode '9' <> 57 then
        failwith "Test Case 4 failed"
    if getAsciiCode 'a' <> 97 then
        failwith "Test Case 5 failed"
    if getAsciiCode 'z' <> 122 then
        failwith "Test Case 6 failed"

check ()