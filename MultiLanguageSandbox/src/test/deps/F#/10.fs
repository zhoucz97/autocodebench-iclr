
let findDestination (paths: (string * string) list) =
    let startingCities = paths |> List.map fst |> Set.ofList
    paths 
    |> List.map snd 
    |> List.find (fun city -> not (Set.contains city startingCities))


let test () =
    if findDestination [("London", "New York"); ("New York", "Tokyo"); ("Tokyo", "Berlin")] <> Some "Berlin" then
        failwith "Test Case 1 failed"
    if findDestination [("Chicago", "Winnipeg"); ("Halifax", "Montreal"); ("Montreal", "Toronto"); ("Toronto", "Chicago")] <> Some "Winnipeg" then
        failwith "Test Case 2 failed"
    if findDestination [("A", "B"); ("B", "C"); ("C", "D")] <> Some "D" then
        failwith "Test Case 3 failed"
    if findDestination [("Paris", "Lyon"); ("Lyon", "Marseille")] <> Some "Marseille" then
        failwith "Test Case 4 failed"

test ()