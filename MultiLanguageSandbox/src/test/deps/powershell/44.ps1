function Multiply-TwoNumbers {
    param (
        [Parameter(Mandatory=$true)]
        [int]$A,
        
        [Parameter(Mandatory=$true)]
        [int]$B
    )
    
    return $A * $B
}


function Test-MultiplyTwoNumbers {
    if ((Multiply-TwoNumbers -A 3 -B 4) -ne 12) { throw "Test case 1 failed" }
    if ((Multiply-TwoNumbers -A 10 -B 20) -ne 200) { throw "Test case 2 failed" }
    if ((Multiply-TwoNumbers -A 50000 -B 1) -ne 50000) { throw "Test case 3 failed" }
    if ((Multiply-TwoNumbers -A 1 -B 50000) -ne 50000) { throw "Test case 4 failed" }
    if ((Multiply-TwoNumbers -A 25000 -B 2) -ne 50000) { throw "Test case 5 failed" }
}

Test-MultiplyTwoNumbers