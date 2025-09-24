
let longestConsecutiveSequence (numbers: int list) =
    let numSet = Set.ofList numbers
    let mutable maxLength = 0
    
    for num in numSet do
        // Check if this number is the start of a sequence
        if not (numSet.Contains(num - 1)) then
            let mutable currentNum = num
            let mutable currentLength = 1
            
            // Count how long the sequence is
            while numSet.Contains(currentNum + 1) do
                currentNum <- currentNum + 1
                currentLength <- currentLength + 1
            
            // Update maxLength if we found a longer sequence
            if currentLength > maxLength then
                maxLength <- currentLength
    
    maxLength


let check () =
    if longestConsecutiveSequence [5; 4; 3; 2; 1; 6; 10; 11] <> 6 then
        failwith "Test Case 1 failed"
    if longestConsecutiveSequence [8; 9; 1; 2; 3; 7; 6; 10] <> 5 then
        failwith "Test Case 2 failed"
    if longestConsecutiveSequence [15; 5; 1; 2; 12; 3; 10; 4] <> 5 then
        failwith "Test Case 3 failed"
    if longestConsecutiveSequence [1; 2; 3; 4; 5; 9; 10; 11; 12] <> 5 then
        failwith "Test Case 4 failed"
    if longestConsecutiveSequence [1; 3; 5; 7; 9; 2; 4; 6; 8; 10] <> 10 then
        failwith "Test Case 5 failed"
    if longestConsecutiveSequence [1] <> 1 then
        failwith "Test Case 6 failed"
    if longestConsecutiveSequence [100; 101; 102; 103; 105; 106; 107] <> 4 then
        failwith "Test Case 7 failed"

check ()