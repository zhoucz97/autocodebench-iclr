function Is-PalindromeNumber {
    param (
        [Parameter(Mandatory=$true)]
        [int]$x
    )

    # Negative numbers can't be palindromes
    if ($x -lt 0) {
        return $false
    }

    # Convert the number to a string
    $str = $x.ToString()

    # Compare characters from start and end moving towards the center
    $left = 0
    $right = $str.Length - 1

    while ($left -lt $right) {
        if ($str[$left] -ne $str[$right]) {
            return $false
        }
        $left++
        $right--
    }

    return $true
}


function Check-PalindromeNumber() {
if ((Is-PalindromeNumber 121) -ne $true) { throw "Test case 1 failed" }
if ((Is-PalindromeNumber -121) -ne $false) { throw "Test case 2 failed" }
if ((Is-PalindromeNumber 10) -ne $false) { throw "Test case 3 failed" }
if ((Is-PalindromeNumber 1221) -ne $true) { throw "Test case 4 failed" }
if ((Is-PalindromeNumber 12321) -ne $true) { throw "Test case 5 failed" }
if ((Is-PalindromeNumber 123321) -ne $true) { throw "Test case 6 failed" }
if ((Is-PalindromeNumber 0) -ne $true) { throw "Test case 7 failed" }
if ((Is-PalindromeNumber 123) -ne $false) { throw "Test case 8 failed" }
if ((Is-PalindromeNumber 1) -ne $true) { throw "Test case 9 failed" }
if ((Is-PalindromeNumber 1000021) -ne $false) { throw "Test case 10 failed" }
}

Check-PalindromeNumber