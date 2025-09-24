
let finalPosition (commands: string) (time: int) : (int * int) =
    let rec move (x, y) index remaining =
        if remaining = 0 then (x, y)
        else
            let cmd = commands.[index % commands.Length]
            let (newX, newY) =
                match cmd with
                | 'E' -> (x + 1, y)
                | 'W' -> (x - 1, y)
                | 'N' -> (x, y + 1)
                | 'S' -> (x, y - 1)
                | _ -> (x, y) // handle unexpected characters, though problem says commands are valid
            move (newX, newY) (index + 1) (remaining - 1)
    move (0, 0) 0 time


let check () =
    if finalPosition "ENWS" 5 <> (1, 0) then
        failwith "Test Case 1 failed"
    if finalPosition "NNSS" 3 <> (0, 1) then
        failwith "Test Case 2 failed"
    if finalPosition "SWNE" 4 <> (0, 0) then
        failwith "Test Case 3 failed"
    if finalPosition "EESSWWNN" 8 <> (0, 0) then
        failwith "Test Case 4 failed"
    if finalPosition "W" 10 <> (-10, 0) then
        failwith "Test Case 5 failed"
    if finalPosition "NESW" 2 <> (1, 1) then
        failwith "Test Case 6 failed"
    if finalPosition "SSSEEE" 6 <> (3, -3) then
        failwith "Test Case 7 failed"

check ()