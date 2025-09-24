function Get-MinesweeperGrid {
    param (
        [Parameter(Mandatory=$true)]
        [int]$n,
        
        [Parameter(Mandatory=$true)]
        [int]$m,
        
        [Parameter(Mandatory=$true)]
        [string[]]$grid
    )

    # Create a new grid to store the results
    $result = New-Object 'string[][]' $n

    # Iterate through each cell in the grid
    for ($i = 0; $i -lt $n; $i++) {
        $row = New-Object 'char[]' $m
        for ($j = 0; $j -lt $m; $j++) {
            if ($grid[$i][$j] -eq '*') {
                # If it's a mine, keep it as is
                $row[$j] = '*'
            } else {
                # Count adjacent mines
                $count = 0
                # Check all 8 possible neighboring cells
                for ($di = -1; $di -le 1; $di++) {
                    for ($dj = -1; $dj -le 1; $dj++) {
                        # Skip the current cell itself
                        if ($di -eq 0 -and $dj -eq 0) { continue }
                        
                        $ni = $i + $di
                        $nj = $j + $dj
                        
                        # Check if the neighbor is within bounds
                        if ($ni -ge 0 -and $ni -lt $n -and $nj -ge 0 -and $nj -lt $m) {
                            if ($grid[$ni][$nj] -eq '*') {
                                $count++
                            }
                        }
                    }
                }
                # Set the count or 0 if no mines
                $row[$j] = if ($count -gt 0) { [char]([int][char]'0' + $count) } else { '0' }
            }
        }
        $result[$i] = -join $row
    }

    return $result
}


# Test cases
function Test-MinesweeperGrid() {
$test1 = Get-MinesweeperGrid -n 3 -m 3 -grid @("*??", "???", "?*?")
$expected1 = @("*10", "221", "1*1")

$test2 = Get-MinesweeperGrid -n 2 -m 3 -grid @("?*?", "*??")
$expected2 = @("2*1", "*21")

if (Compare-Object $test1 $expected1) { throw "Test case 1 failed" }
if (Compare-Object $test2 $expected2) { throw "Test case 2 failed" }
}

Test-MinesweeperGrid