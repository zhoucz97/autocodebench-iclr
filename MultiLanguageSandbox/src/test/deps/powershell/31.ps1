function Calculate-ModelSurfaceArea {
    param (
        [Parameter(Mandatory=$true)]
        [int]$n,
        
        [Parameter(Mandatory=$true)]
        [int]$m,
        
        [Parameter(Mandatory=$true)]
        [int[][]]$grid
    )

    $totalSurfaceArea = 0

    # Directions: up, down, left, right
    $directions = @(
        @(0, 1),  # right
        @(1, 0),  # down
        @(0, -1), # left
        @(-1, 0)  # up
    )

    for ($i = 0; $i -lt $n; $i++) {
        for ($j = 0; $j -lt $m; $j++) {
            $currentHeight = $grid[$i][$j]
            
            # Top and bottom surfaces (always contribute 2 units per cube)
            if ($currentHeight -gt 0) {
                $totalSurfaceArea += 2
            }
            
            # Check four neighboring cells
            foreach ($dir in $directions) {
                $ni = $i + $dir[0]
                $nj = $j + $dir[1]
                
                # If neighbor is out of bounds, it's air (height 0)
                $neighborHeight = 0
                if ($ni -ge 0 -and $ni -lt $n -and $nj -ge 0 -and $nj -lt $m) {
                    $neighborHeight = $grid[$ni][$nj]
                }
                
                # Add the difference between current and neighbor height
                $diff = $currentHeight - $neighborHeight
                if ($diff -gt 0) {
                    $totalSurfaceArea += $diff
                }
            }
        }
    }

    return $totalSurfaceArea
}


function Test-ModelSurfaceArea {
if ((Calculate-ModelSurfaceArea 3 3 @(@(1,1,1), @(2,1,2), @(1,1,1))) -ne 38) { throw "Test case 1 failed" }
if ((Calculate-ModelSurfaceArea 2 2 @(@(1,0), @(0,1))) -ne 12) { throw "Test case 2 failed" }
if ((Calculate-ModelSurfaceArea 2 3 @(@(2,3,2), @(1,0,1))) -ne 34) { throw "Test case 3 failed" }
if ((Calculate-ModelSurfaceArea 4 4 @(@(0,0,0,0), @(0,2,2,0), @(0,2,2,0), @(0,0,0,0))) -ne 24) { throw "Test case 4 failed" }
}

Test-ModelSurfaceArea