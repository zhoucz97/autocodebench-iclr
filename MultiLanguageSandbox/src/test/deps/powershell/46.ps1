function Reverse-ThreeDigitNumber {
    param (
        [Parameter(Mandatory=$true)]
        [int]$n
    )
    
    # Validate that the input is a three-digit number
    if ($n -lt 100 -or $n -gt 999) {
        throw "Input must be a three-digit number (100-999)"
    }
    
    # Convert the number to a string to easily reverse the digits
    $str = $n.ToString()
    
    # Reverse the string
    $reversedStr = -join ($str.ToCharArray() | ForEach-Object { $_ } | Sort-Object { -$_ })
    
    # Return the reversed string (which will maintain leading zeros)
    return $reversedStr
}


function Test-ReverseThreeDigitNumber {
    if ((Reverse-ThreeDigitNumber -n 100) -ne 001) { throw "Test case 1 failed" }
    if ((Reverse-ThreeDigitNumber -n 678) -ne 876) { throw "Test case 2 failed" }
    if ((Reverse-ThreeDigitNumber -n 987) -ne 789) { throw "Test case 3 failed" }
    if ((Reverse-ThreeDigitNumber -n 321) -ne 123) { throw "Test case 4 failed" }
    if ((Reverse-ThreeDigitNumber -n 123) -ne 321) { throw "Test case 5 failed" }
}

Test-ReverseThreeDigitNumber