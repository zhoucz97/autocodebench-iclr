function Get-PowerOfTwo {
    param (
        [Parameter(Mandatory=$true)]
        [ValidateScript({$_ -ge 0})]
        [int]$n
    )
    
    return [math]::Pow(2, $n)
}


function Test-GetPowerOfTwo {
    if ((Get-PowerOfTwo -n 3) -ne 8) { throw "Test case 1 failed" }
    if ((Get-PowerOfTwo -n 0) -ne 1) { throw "Test case 2 failed" }
    if ((Get-PowerOfTwo -n 10) -ne 1024) { throw "Test case 3 failed" }
    if ((Get-PowerOfTwo -n 15) -ne 32768) { throw "Test case 4 failed" }
    if ((Get-PowerOfTwo -n 20) -ne 1048576) { throw "Test case 5 failed" }
}

Test-GetPowerOfTwo