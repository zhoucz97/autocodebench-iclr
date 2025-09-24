function Get-ASCIICode {
    param (
        [Parameter(Mandatory=$true)]
        [char]$char
    )
    
    return [int][char]::ConvertToUtf32($char, 0)
}


function Test-GetASCIICode {
    if ((Get-ASCIICode -char 'A') -ne 65) { throw "Test case 1 failed" }
    if ((Get-ASCIICode -char 'a') -ne 97) { throw "Test case 2 failed" }
    if ((Get-ASCIICode -char '0') -ne 48) { throw "Test case 3 failed" }
    if ((Get-ASCIICode -char '!') -ne 33) { throw "Test case 4 failed" }
    if ((Get-ASCIICode -char '~') -ne 126) { throw "Test case 5 failed" }
}

Test-GetASCIICode