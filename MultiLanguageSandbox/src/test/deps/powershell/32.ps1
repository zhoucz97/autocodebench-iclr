function Calculate-WaterCollectionTime {
    param (
        [Parameter(Mandatory=$true)]
        [int]$n,
        
        [Parameter(Mandatory=$true)]
        [int]$m,
        
        [Parameter(Mandatory=$true)]
        [int[]]$waterAmounts
    )
    
    # Sort the water amounts in descending order
    $sortedWater = $waterAmounts | Sort-Object -Descending
    
    $totalTime = 0
    $index = 0
    
    while ($index -lt $n) {
        # Determine the number of taps to use in this round (could be less than m if remaining students < m)
        $tapsInThisRound = [Math]::Min($m, $n - $index)
        
        # The time taken for this round is the maximum in the current group
        $maxInGroup = $sortedWater[$index]
        $totalTime += $maxInGroup
        
        # Move the index forward by the number of taps used in this round
        $index += $tapsInThisRound
    }
    
    return $totalTime
}


function Test-WaterCollectionTime {
if ((Calculate-WaterCollectionTime 4 2 @(3, 3, 4, 4)) -ne 7) { throw "Test case 1 failed" }
if ((Calculate-WaterCollectionTime 3 1 @(2, 3, 4)) -ne 9) { throw "Test case 2 failed" }
if ((Calculate-WaterCollectionTime 6 3 @(1, 2, 2, 3, 1, 4)) -ne 6) { throw "Test case 3 failed" }
if ((Calculate-WaterCollectionTime 2 2 @(5, 6)) -ne 6) { throw "Test case 4 failed" }
if ((Calculate-WaterCollectionTime 5 3 @(1, 1, 1, 1, 1)) -ne 2) { throw "Test case 5 failed" }
}

Test-WaterCollectionTime