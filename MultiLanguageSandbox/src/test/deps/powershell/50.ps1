function Calculate-Expression {
    param (
        [int]$a,
        [int]$b,
        [int]$c
    )
    
    return ($a + $b) * $c
}


function Test-CalculateExpression {
    if ((Calculate-Expression -a 2 -b 3 -c 5) -ne 25) { throw "Test case 1 failed" }
    if ((Calculate-Expression -a 1 -b 1 -c 1) -ne 2) { throw "Test case 2 failed" }
    if ((Calculate-Expression -a 0 -b 0 -c 0) -ne 0) { throw "Test case 3 failed" }
    if ((Calculate-Expression -a -1 -b -1 -c -1) -ne 2) { throw "Test case 4 failed" }
    if ((Calculate-Expression -a 10000 -b 10000 -c 10000) -ne 200000000) { throw "Test case 5 failed" }
    if ((Calculate-Expression -a -10000 -b -10000 -c -10000) -ne 200000000) { throw "Test case 6 failed" }
}