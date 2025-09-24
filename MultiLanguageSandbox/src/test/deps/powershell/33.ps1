function Calculate-WaysToClimb {
    param (
        [Parameter(Mandatory=$true)]
        [int]$N,
        
        [Parameter(Mandatory=$true)]
        [int]$K
    )

    # Handle edge cases
    if ($N -eq 0) { return 1 }
    if ($K -eq 0) { return 0 }

    # Initialize an array to store the number of ways to reach each step
    $dp = New-Object int[] ($N + 1)
    $dp[0] = 1  # There's one way to stay at ground level (do nothing)

    for ($i = 1; $i -le $N; $i++) {
        for ($j = 1; $j -le $K; $j++) {
            if ($i -ge $j) {
                $dp[$i] = ($dp[$i] + $dp[$i - $j]) % 100003
            }
        }
    }

    return $dp[$N]
}


# Test cases
function Test-WaysToClimb {
if ((Calculate-WaysToClimb 4 2) -ne 5) { throw "Test case 1 failed" }
if ((Calculate-WaysToClimb 3 3) -ne 4) { throw "Test case 2 failed" }
if ((Calculate-WaysToClimb 6 3) -ne 24) { throw "Test case 3 failed" }
if ((Calculate-WaysToClimb 7 4) -ne 56) { throw "Test case 4 failed" }
if ((Calculate-WaysToClimb 10 5) -ne 464) { throw "Test case 5 failed" }
}

Test-WaysToClimb