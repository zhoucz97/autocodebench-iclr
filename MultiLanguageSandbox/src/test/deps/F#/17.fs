
let averageSalaryExcludingMinMax (salary: int list) =
    if List.length salary <= 2 then
        0.0 // or handle this case differently if needed
    else
        let minSalary = List.min salary
        let maxSalary = List.max salary
        let filteredSalaries = 
            salary 
            |> List.filter (fun x -> x <> minSalary && x <> maxSalary)
        float (List.sum filteredSalaries) / float (List.length filteredSalaries)


let test () =
    if averageSalaryExcludingMinMax [4000; 3000; 1000; 2000] <> 2500.0 then
        failwith "Test Case 1 failed"
    if averageSalaryExcludingMinMax [6000; 5000; 4000; 3000; 2000; 1000] <> 3500.0 then
        failwith "Test Case 2 failed"
    if averageSalaryExcludingMinMax [8000; 7000; 6000; 5000; 4000] <> 6000.0 then
        failwith "Test Case 3 failed"
    if averageSalaryExcludingMinMax [3000; 2000; 1000] <> 2000.0 then
        failwith "Test Case 4 failed"
    if averageSalaryExcludingMinMax [5000; 3000; 2000; 1000; 4000] <> 3000.0 then
        failwith "Test Case 5 failed"

test ()