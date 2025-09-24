function Get-TotalCoins {
    param (
        [Parameter(Mandatory=$true)]
        [int]$days
    )

    $totalCoins = 0
    $currentCoins = 1
    $remainingDaysInPhase = $currentCoins

    for ($day = 1; $day -le $days; $day++) {
        $totalCoins += $currentCoins
        $remainingDaysInPhase--

        if ($remainingDaysInPhase -eq 0) {
            $currentCoins++
            $remainingDaysInPhase = $currentCoins
        }
    }

    return $totalCoins
}


function Test-TotalCoins() {
if ((Get-TotalCoins -days 7) -ne 18) { throw "Test case 1 failed" }
if ((Get-TotalCoins -days 10) -ne 30) { throw "Test case 2 failed" }
if ((Get-TotalCoins -days 15) -ne 55) { throw "Test case 3 failed" }
if ((Get-TotalCoins -days 20) -ne 85) { throw "Test case 4 failed" }
if ((Get-TotalCoins -days 1) -ne 1) { throw "Test case 5 failed" }
if ((Get-TotalCoins -days 3) -ne 5) { throw "Test case 6 failed" }
if ((Get-TotalCoins -days 5) -ne 11) { throw "Test case 7 failed" }
}

Test-TotalCoins