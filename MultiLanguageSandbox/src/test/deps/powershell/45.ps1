function Get-ArithmeticSeriesTerm {
    param (
        [double]$a1,  # First term
        [double]$a2,  # Second term
        [int]$n       # Term number to find
    )

    # Calculate the common difference (d)
    $d = $a2 - $a1

    # Calculate the nth term using the formula: a_n = a1 + (n-1)*d
    $nthTerm = $a1 + ($n - 1) * $d

    return $nthTerm
}

# Example usage:
Get-ArithmeticSeriesTerm -a1 1 -a2 4 -n 100


function Test-GetArithmeticSeriesTerm {
    if ((Get-ArithmeticSeriesTerm -a1 1 -a2 4 -n 100) -ne 298) { throw "Test case 1 failed" }
    if ((Get-ArithmeticSeriesTerm -a1 5 -a2 10 -n 50) -ne 250) { throw "Test case 2 failed" }
    if ((Get-ArithmeticSeriesTerm -a1 100 -a2 99 -n 50) -ne 51) { throw "Test case 3 failed" }
    if ((Get-ArithmeticSeriesTerm -a1 0 -a2 0 -n 1000) -ne 0) { throw "Test case 4 failed" }
}

Test-GetArithmeticSeriesTerm