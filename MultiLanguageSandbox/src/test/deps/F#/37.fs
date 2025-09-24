
let maxContests (contests: (int * int) list) =
    let sortedContests = List.sortBy snd contests
    let rec loop remaining lastEnd count =
        match remaining with
        | [] -> count
        | (start, end_) :: rest ->
            if start >= lastEnd then
                loop rest end_ (count + 1)
            else
                loop rest lastEnd count
    match sortedContests with
    | [] -> 0
    | (start, end_) :: rest -> loop rest end_ 1


let check () =
    if maxContests [(1, 3); (2, 5); (4, 6)] <> 2 then
        failwith "Test Case 1 failed"
    if maxContests [(0, 2); (1, 3); (3, 5)] <> 2 then
        failwith "Test Case 2 failed"
    if maxContests [(5, 7); (1, 3); (2, 4)] <> 2 then
        failwith "Test Case 3 failed"
    if maxContests [(1, 4); (2, 5); (6, 8)] <> 2 then
        failwith "Test Case 4 failed"
    if maxContests [(0, 1); (1, 2); (2, 3)] <> 3 then
        failwith "Test Case 5 failed"

check ()