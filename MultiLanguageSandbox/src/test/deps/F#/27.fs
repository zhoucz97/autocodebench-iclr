
let rec calculateBessieScore (n: int) : int =
    if n = 1 then
        0
    else
        let nextN = 
            if n % 2 = 1 then
                3 * n + 1
            else
                n / 2
        1 + calculateBessieScore nextN


let test () =
    if calculateBessieScore 6 <> 8 then
        failwith "Test Case 1 failed"
    if calculateBessieScore 1 <> 0 then
        failwith "Test Case 2 failed"
    if calculateBessieScore 7 <> 16 then
        failwith "Test Case 3 failed"
    if calculateBessieScore 15 <> 17 then
        failwith "Test Case 4 failed"
    if calculateBessieScore 22 <> 15 then
        failwith "Test Case 5 failed"
    if calculateBessieScore 2 <> 1 then
        failwith "Test Case 6 failed"
    if calculateBessieScore 10 <> 6 then
        failwith "Test Case 7 failed"

test ()