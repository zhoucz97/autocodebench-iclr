function Get-ASCIIChar {
    param (
        [int]$asciiCode
    )
    
    return [char]$asciiCode
}


function Test-GetASCIIChar {
    if ((Get-ASCIIChar -asciiCode 65) -ne 'A') { throw "Test case 1 failed" }
    if ((Get-ASCIIChar -asciiCode 97) -ne 'a') { throw "Test case 2 failed" }
    if ((Get-ASCIIChar -asciiCode 48) -ne '0') { throw "Test case 3 failed" }
    if ((Get-ASCIIChar -asciiCode 33) -ne '!') { throw "Test case 4 failed" }
    if ((Get-ASCIIChar -asciiCode 126) -ne '~') { throw "Test case 5 failed" }
}

Test-GetASCIIChar