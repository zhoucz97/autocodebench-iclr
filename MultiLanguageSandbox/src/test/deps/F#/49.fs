
let determineSign (n: int): string =
    if n > 0 then "positive"
    elif n = 0 then "zero"
    else "negative"


let check () =
    if determineSign 10 <> "positive" then
        failwith "Test Case 1 failed"
    if determineSign 0 <> "zero" then
        failwith "Test Case 2 failed"
    if determineSign -5 <> "negative" then
        failwith "Test Case 3 failed"
    if determineSign 100 <> "positive" then
        failwith "Test Case 4 failed"
    if determineSign -100 <> "negative" then
        failwith "Test Case 5 failed"
    if determineSign 1 <> "positive" then
        failwith "Test Case 6 failed"

check ()