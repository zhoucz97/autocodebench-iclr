function Get-StudentIDsByOrder {
    param (
        [Parameter(Mandatory=$true)]
        [int[]]$studentIDs,
        
        [Parameter(Mandatory=$true)]
        [int[]]$queries
    )
    
    # Create a hashtable to map query positions to student IDs
    $result = @()
    
    foreach ($query in $queries) {
        # Check if the query is within valid range (1-based index)
        if ($query -ge 1 -and $query -le $studentIDs.Count) {
            $result += $studentIDs[$query - 1]  # Convert to 0-based index
        }
        else {
            # Handle invalid query (optional - could throw an error instead)
            Write-Warning "Query position $query is out of range (1-$($studentIDs.Count))"
        }
    }
    
    return $result
}


function Test-StudentIDsByOrder() {
$result1 = Get-StudentIDsByOrder -studentIDs @(1, 9, 2, 60, 8, 17, 11, 4, 5, 14) -queries @(1, 5, 9)
if (Compare-Object $result1 @(1, 8, 5)) { throw "Test case 1 failed" }

$result2 = Get-StudentIDsByOrder -studentIDs @(7, 15, 3, 22, 9) -queries @(2, 4)
if (Compare-Object $result2 @(15, 22)) { throw "Test case 2 failed" }
}

Test-StudentIDsByOrder