function Get-MinimumSegments {
    param (
        [Parameter(Mandatory=$true)]
        [int[]]$numbers,
        
        [Parameter(Mandatory=$true)]
        [int]$maxSum
    )

    $segments = 0
    $currentSum = 0

    foreach ($num in $numbers) {
        if ($currentSum + $num > $maxSum) {
            # Need to start a new segment
            $segments++
            $currentSum = $num
        } else {
            # Can add to current segment
            $currentSum += $num
        }
    }

    # Add the last segment if there are any numbers left
    if ($currentSum -gt 0) {
        $segments++
    }

    return $segments
}


function Test-MinimumSegments() {
if ((Get-MinimumSegments -numbers @(4, 2, 4, 5, 1) -maxSum 6) -ne 3) { throw "Test case 1 failed" }
if ((Get-MinimumSegments -numbers @(1, 2, 3, 4, 5) -maxSum 10) -ne 2) { throw "Test case 2 failed" }
if ((Get-MinimumSegments -numbers @(10, 10, 10) -maxSum 10) -ne 3) { throw "Test case 3 failed" }
}

Test-MinimumSegments