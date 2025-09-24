
let findLargerPrimeFactor (n: int) =
    let rec isPrime x =
        let rec check i =
            if i * i > x then true
            elif x % i = 0 then false
            else check (i + 1)
        if x < 2 then false
        else check 2
    
    let rec findSmallerFactor x divisor =
        if divisor * divisor > x then x
        elif x % divisor = 0 && isPrime divisor then divisor
        else findSmallerFactor x (divisor + 1)
    
    let smallerFactor = findSmallerFactor n 2
    n / smallerFactor


let testFindLargerPrimeFactor () =
    if findLargerPrimeFactor 15 <> 5 then
        failwith "Test Case 1 failed"
    if findLargerPrimeFactor 35 <> 7 then
        failwith "Test Case 2 failed"
    if findLargerPrimeFactor 77 <> 11 then
        failwith "Test Case 3 failed"
    if findLargerPrimeFactor 143 <> 13 then
        failwith "Test Case 4 failed"
    if findLargerPrimeFactor 221 <> 17 then
        failwith "Test Case 5 failed"
    if findLargerPrimeFactor 323 <> 19 then
        failwith "Test Case 6 failed"
    if findLargerPrimeFactor 437 <> 23 then
        failwith "Test Case 7 failed"

testFindLargerPrimeFactor ()