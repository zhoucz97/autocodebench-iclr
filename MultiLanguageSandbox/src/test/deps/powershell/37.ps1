function Get-SpecialPrimes {
    param (
        [Parameter(Mandatory=$true)]
        [int]$length
    )

    # Helper function to check if a number is prime
    function IsPrime($n) {
        if ($n -le 1) { return $false }
        if ($n -eq 2) { return $true }
        if ($n % 2 -eq 0) { return $false }
        
        $sqrtN = [math]::Sqrt($n)
        for ($i = 3; $i -le $sqrtN; $i += 2) {
            if ($n % $i -eq 0) { return $false }
        }
        return $true
    }

    # Helper function to generate special primes recursively
    function GenerateSpecialPrimes($current, $remainingLength) {
        if ($remainingLength -eq 0) {
            return @($current)
        }

        $result = @()
        $startDigit = if ($current -eq 0) { 2 } else { 1 } # First digit can't be 0 or 1
        $endDigit = 9

        for ($digit = $startDigit; $digit -le $endDigit; $digit++) {
            $newNumber = $current * 10 + $digit
            if (IsPrime $newNumber) {
                $result += GenerateSpecialPrimes $newNumber ($remainingLength - 1)
            }
        }

        return $result
    }

    # Validate input
    if ($length -lt 1) {
        throw "Length must be a positive integer"
    }

    # Start generating from single-digit primes (2, 3, 5, 7)
    $singleDigitPrimes = @(2, 3, 5, 7)
    $specialPrimes = @()

    foreach ($prime in $singleDigitPrimes) {
        $specialPrimes += GenerateSpecialPrimes $prime ($length - 1)
    }

    # Filter to only include numbers with exactly the requested length
    $filteredPrimes = $specialPrimes | Where-Object { $_ -ge [math]::Pow(10, $length - 1) -and $_ -lt [math]::Pow(10, $length) }

    return $filteredPrimes
}


function Test-SpecialPrimes() {
$result1 = Get-SpecialPrimes 2
if (-not (Compare-Object $result1 @(23, 29, 31, 37, 53, 59, 71, 73, 79, 97))) { throw "Test case 1 failed" }

$result2 = Get-SpecialPrimes 3
if ($result2.Count -eq 0) { throw "Test case 2 failed: No special primes found" }
}

Test-SpecialPrimes