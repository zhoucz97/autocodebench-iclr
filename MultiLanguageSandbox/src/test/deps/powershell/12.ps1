function Max-WaterContainer {
    param (
        [Parameter(Mandatory=$true)]
        [System.Collections.Generic.List[int]]$height
    )

    $left = 0
    $right = $height.Count - 1
    $maxArea = 0

    while ($left -lt $right) {
        $currentHeight = [Math]::Min($height[$left], $height[$right])
        $currentWidth = $right - $left
        $currentArea = $currentHeight * $currentWidth
        
        if ($currentArea -gt $maxArea) {
            $maxArea = $currentArea
        }

        if ($height[$left] -lt $height[$right]) {
            $left++
        } else {
            $right--
        }
    }

    return $maxArea
}


function Check-MaxWaterContainer() {
if ((Max-WaterContainer @([int[]](1, 8, 6, 2, 5, 4, 8, 3, 7))) -ne 49) { throw "Test case 1 failed" }
if ((Max-WaterContainer @([int[]](1, 1))) -ne 1) { throw "Test case 2 failed" }
if ((Max-WaterContainer @([int[]](4, 3, 2, 1, 4))) -ne 16) { throw "Test case 3 failed" }
if ((Max-WaterContainer @([int[]](1, 2, 4, 3))) -ne 4) { throw "Test case 4 failed" }
if ((Max-WaterContainer @([int[]](1, 8, 6, 2, 5, 4, 8, 25, 7))) -ne 49) { throw "Test case 5 failed" }
if ((Max-WaterContainer @([int[]](1, 3, 2, 5, 25, 24, 5))) -ne 24) { throw "Test case 6 failed" }
}

Check-MaxWaterContainer