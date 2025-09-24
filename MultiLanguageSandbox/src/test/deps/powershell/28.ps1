function Get-DistinctRemainders {
    param (
        [Parameter(Mandatory=$true)]
        [int[]]$numbers
    )
    
    # Create a hashtable to store unique remainders
    $remainders = @{}
    
    foreach ($number in $numbers) {
        $remainder = $number % 42
        $remainders[$remainder] = $true
    }
    
    # Return the count of unique remainders
    return $remainders.Count
}


function Test-GetDistinctRemainders() {
if ((Get-DistinctRemainders -numbers @(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) -ne 10) { throw "Test case 1 failed" }
if ((Get-DistinctRemainders -numbers @(42, 84, 252, 420, 840, 126, 42, 84, 420, 126)) -ne 1) { throw "Test case 2 failed" }
if ((Get-DistinctRemainders -numbers @(39, 40, 41, 42, 43, 44, 82, 83, 84, 85)) -ne 6) { throw "Test case 3 failed" }
}

Test-GetDistinctRemainders