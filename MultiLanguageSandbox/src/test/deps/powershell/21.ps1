function Get-MinimumCowsForHeight {
    param (
        [Parameter(Mandatory=$true)]
        [int]$shelfHeight,
        
        [Parameter(Mandatory=$true)]
        [System.Collections.Generic.List[int]]$cowHeights
    )

    # Sort the cow heights in descending order to use the tallest cows first
    $sortedCows = $cowHeights | Sort-Object -Descending
    
    $totalHeight = 0
    $cowsUsed = 0
    
    foreach ($height in $sortedCows) {
        $totalHeight += $height
        $cowsUsed++
        
        if ($totalHeight -ge $shelfHeight) {
            return $cowsUsed
        }
    }
    
    # If we've used all cows and still haven't reached the height, return -1 (or handle as needed)
    return -1
}


function Test-MinimumCowsForHeight() {
if ((Get-MinimumCowsForHeight -shelfHeight 30 -cowHeights (New-Object 'System.Collections.Generic.List[int]' (,([int[]](10, 12, 8, 5))))) -ne 3) { throw "Test case 1 failed" }
if ((Get-MinimumCowsForHeight -shelfHeight 50 -cowHeights (New-Object 'System.Collections.Generic.List[int]' (,([int[]](15, 20, 10, 18))))) -ne 3) { throw "Test case 2 failed" }
if ((Get-MinimumCowsForHeight -shelfHeight 45 -cowHeights (New-Object 'System.Collections.Generic.List[int]' (,([int[]](9, 15, 8, 17, 6))))) -ne 4) { throw "Test case 3 failed" }
if ((Get-MinimumCowsForHeight -shelfHeight 70 -cowHeights (New-Object 'System.Collections.Generic.List[int]' (,([int[]](22, 27, 15, 10, 18))))) -ne 4) { throw "Test case 4 failed" }
}

Test-MinimumCowsForHeight