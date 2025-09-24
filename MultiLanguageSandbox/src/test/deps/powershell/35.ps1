function Get-MaxPathSum {
    param (
        [Parameter(Mandatory=$true)]
        [int[][]]$pyramid
    )

    # Create a copy of the pyramid to store maximum sums
    $dp = @()
    for ($i = 0; $i -lt $pyramid.Length; $i++) {
        $dp += ,@($pyramid[$i])
    }

    # Start from the second last row and move upwards
    for ($i = $pyramid.Length - 2; $i -ge 0; $i--) {
        for ($j = 0; $j -lt $pyramid[$i].Length; $j++) {
            # For each element, add the maximum of the two adjacent elements from the row below
            $dp[$i][$j] += [Math]::Max($dp[$i + 1][$j], $dp[$i + 1][$j + 1])
        }
    }

    # The top element now contains the maximum path sum
    return $dp[0][0]
}

# Example usage:
$pyramid = @( @(1), @(2, 3), @(4, 5, 6) )
Get-MaxPathSum $pyramid  # Should return 10


function Test-MaxPathSum() {
$pyramid1 = @( @(3), @(7, 4), @(2, 4, 6), @(8, 5, 9, 3) )
$result1 = Get-MaxPathSum $pyramid1
if ($result1 -ne 23) { throw "Test case 1 failed: Expected 23, got $result1" }

$pyramid2 = @( @(1), @(2, 1), @(1, 2, 1) )
$result2 = Get-MaxPathSum $pyramid2
if ($result2 -ne 5) { throw "Test case 2 failed: Expected 4, got $result2" }

$pyramid3 = @( @(1) )
$result3 = Get-MaxPathSum $pyramid3
if ($result3 -ne 1) { throw "Test case 3 failed: Expected 1, got $result3" }
}

Test-MaxPathSum