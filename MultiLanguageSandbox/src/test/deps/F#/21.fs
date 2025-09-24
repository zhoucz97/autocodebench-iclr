
let calculateSwimmingTime (startHour: int) (startMinute: int) (endHour: int) (endMinute: int) =
    // Convert start and end times to total minutes since midnight
    let startTotal = startHour * 60 + startMinute
    let endTotal = endHour * 60 + endMinute
    
    // Calculate the difference in minutes
    let diff = if endTotal >= startTotal then endTotal - startTotal else (24 * 60) - startTotal + endTotal
    
    // Convert back to hours and minutes
    let hours = diff / 60
    let minutes = diff % 60
    
    (hours, minutes)


let check () =
    if calculateSwimmingTime 9 30 10 45 <> (1, 15) then
        failwith "Test Case 1 failed"
    if calculateSwimmingTime 23 15 1 30 <> (2, 15) then
        failwith "Test Case 2 failed"
    if calculateSwimmingTime 5 0 5 30 <> (0, 30) then
        failwith "Test Case 3 failed"
    if calculateSwimmingTime 11 20 13 45 <> (2, 25) then
        failwith "Test Case 4 failed"
    if calculateSwimmingTime 16 50 16 50 <> (0, 0) then
        failwith "Test Case 5 failed"
    if calculateSwimmingTime 0 0 23 59 <> (23, 59) then
        failwith "Test Case 6 failed"
    if calculateSwimmingTime 22 15 3 10 <> (4, 55) then
        failwith "Test Case 7 failed"

check ()