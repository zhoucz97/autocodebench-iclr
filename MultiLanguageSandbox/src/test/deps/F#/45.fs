
let reverseThreeDigitNumber (n: int): int =
    let hundreds = n / 100
    let tens = (n / 10) % 10
    let units = n % 10
    units * 100 + tens * 10 + hundreds


let check () =
    if reverseThreeDigitNumber 100 <> 1 then
        failwith "Test Case 1 failed"
    if reverseThreeDigitNumber 250 <> 52 then
        failwith "Test Case 2 failed"
    if reverseThreeDigitNumber 678 <> 876 then
        failwith "Test Case 3 failed"
    if reverseThreeDigitNumber 321 <> 123 then
        failwith "Test Case 4 failed"
    if reverseThreeDigitNumber 909 <> 909 then
        failwith "Test Case 5 failed"
    if reverseThreeDigitNumber 111 <> 111 then
        failwith "Test Case 6 failed"

check ()