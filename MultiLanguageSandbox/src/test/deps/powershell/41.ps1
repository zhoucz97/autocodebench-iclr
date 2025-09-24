function Get-AbsoluteValue {
    param (
        [int]$n
    )
    
    return [Math]::Abs($n)
}


function Test-GetAbsoluteValue {
    if ((Get-AbsoluteValue -n 5) -ne 5) { throw "Test case 1 failed" }
    if ((Get-AbsoluteValue -n -10) -ne 10) { throw "Test case 2 failed" }
    if ((Get-AbsoluteValue -n 0) -ne 0) { throw "Test case 3 failed" }
    if ((Get-AbsoluteValue -n -20000) -ne 20000) { throw "Test case 4 failed" }
    if ((Get-AbsoluteValue -n 10000) -ne 10000) { throw "Test case 5 failed" }
}

Test-GetAbsoluteValue