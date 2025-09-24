function Calculate-ModPower {
    param (
        [Parameter(Mandatory=$true)]
        [int]$a,
        
        [Parameter(Mandatory=$true)]
        [int]$b,
        
        [Parameter(Mandatory=$true)]
        [int]$p
    )

    # Handle the case where p is 1 (any number mod 1 is 0)
    if ($p -eq 1) {
        return 0
    }

    $result = 1
    $a = $a % $p  # Ensure a is within the range of p

    while ($b -gt 0) {
        # If b is odd, multiply a with result
        if (($b -band 1) -eq 1) {
            $result = ($result * $a) % $p
        }

        # b must be even now, so square a and halve b
        $a = ($a * $a) % $p
        $b = $b -shr 1
    }

    return $result
}


function Test-ModPower() {
$result1 = Calculate-ModPower 2 10 9
if ($result1 -ne 7) { throw "Test case 1 failed: Expected 2^10 mod 9=7, got $result1" }

$result2 = Calculate-ModPower 3 7 5
if ($result2 -ne 2) { throw "Test case 2 failed: Expected 3^7 mod 5=2, got $result2" }

$result3 = Calculate-ModPower 5 3 4
if ($result3 -ne 1) { throw "Test case 3 failed: Expected 5^3 mod 4=1, got $result3" }
}

Test-ModPower