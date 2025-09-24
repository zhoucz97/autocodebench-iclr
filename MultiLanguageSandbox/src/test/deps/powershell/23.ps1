function Get-MostFrequentDiceSum {
    param (
        [Parameter(Mandatory=$true)]
        [int]$s1,
        
        [Parameter(Mandatory=$true)]
        [int]$s2,
        
        [Parameter(Mandatory=$true)]
        [int]$s3
    )

    # Initialize a hashtable to count sum frequencies
    $sumCounts = @{}

    # Iterate through all possible combinations of the three dice
    for ($i = 1; $i -le $s1; $i++) {
        for ($j = 1; $j -le $s2; $j++) {
            for ($k = 1; $k -le $s3; $k++) {
                $sum = $i + $j + $k
                
                # Increment the count for this sum
                if ($sumCounts.ContainsKey($sum)) {
                    $sumCounts[$sum]++
                } else {
                    $sumCounts[$sum] = 1
                }
            }
        }
    }

    # Find the maximum frequency
    $maxFrequency = ($sumCounts.Values | Measure-Object -Maximum).Maximum

    # Get all sums with the maximum frequency
    $mostFrequentSums = $sumCounts.GetEnumerator() | 
                        Where-Object { $_.Value -eq $maxFrequency } | 
                        Select-Object -ExpandProperty Key

    # Return the smallest sum among the most frequent ones
    return ($mostFrequentSums | Measure-Object -Minimum).Minimum
}


function Test-MostFrequentDiceSum() {
if ((Get-MostFrequentDiceSum -s1 3 -s2 2 -s3 3) -ne 5) { throw "Test case 1 failed" }
if ((Get-MostFrequentDiceSum -s1 4 -s2 3 -s3 6) -ne 8) { throw "Test case 2 failed" }
if ((Get-MostFrequentDiceSum -s1 6 -s2 6 -s3 6) -ne 10) { throw "Test case 3 failed" }
if ((Get-MostFrequentDiceSum -s1 2 -s2 2 -s3 2) -ne 4) { throw "Test case 4 failed" }
}

Test-MostFrequentDiceSum