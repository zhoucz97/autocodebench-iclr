function Test-NumberSign {
    param (
        [int]$n
    )
    
    if ($n -gt 0) {
        return "positive"
    }
    elseif ($n -lt 0) {
        return "negative"
    }
    else {
        return "zero"
    }
}


function Test-TestNumberSign {
    if ((Test-NumberSign -n 5) -ne "positive") { throw "Test case 1 failed" }
    if ((Test-NumberSign -n -10) -ne "negative") { throw "Test case 2 failed" }
    if ((Test-NumberSign -n 0) -ne "zero") { throw "Test case 3 failed" }
    if ((Test-NumberSign -n 1000000000) -ne "positive") { throw "Test case 4 failed" }
    if ((Test-NumberSign -n -1000000000) -ne "negative") { throw "Test case 5 failed" }
}

Test-TestNumberSign