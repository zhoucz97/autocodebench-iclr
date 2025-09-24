
let reverseDigits (n: int) =
    let rec reverseHelper num acc =
        if num = 0 then acc
        else reverseHelper (num / 10) (acc * 10 + num % 10)
    
    if n < 0 then
        - (reverseHelper (-n) 0)
    else
        reverseHelper n 0


let testReverseDigits () =
    if reverseDigits 123 <> 321 then
        failwith "Test Case 1 failed"
    if reverseDigits -380 <> -83 then
        failwith "Test Case 2 failed"
    if reverseDigits 0 <> 0 then
        failwith "Test Case 3 failed"
    if reverseDigits 100 <> 1 then
        failwith "Test Case 4 failed"
    if reverseDigits -100 <> -1 then
        failwith "Test Case 5 failed"
    if reverseDigits 123456789 <> 987654321 then
        failwith "Test Case 6 failed"
    if reverseDigits -987654321 <> -123456789 then
        failwith "Test Case 7 failed"

testReverseDigits ()