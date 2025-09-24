function Get-MaxPens {
    param (
        [int]$a,  # Yuan
        [int]$b   # Jiao
    )

    # Convert total money to Yuan (since 1 Yuan = 10 Jiao)
    $totalYuan = $a + ($b / 10)

    # Price of one pen in Yuan
    $penPrice = 1.9

    # Calculate maximum number of pens (using integer division)
    $maxPens = [math]::Floor($totalYuan / $penPrice)

    return $maxPens
}


function Test-GetMaxPens {
    if ((Get-MaxPens -a 10 -b 3) -ne 5) { throw "Test case 1 failed" }
    if ((Get-MaxPens -a 20 -b 5) -ne 10) { throw "Test case 2 failed" }
    if ((Get-MaxPens -a 5 -b 9) -ne 3) { throw "Test case 3 failed" }
    if ((Get-MaxPens -a 0 -b 19) -ne 1) { throw "Test case 4 failed" }
    if ((Get-MaxPens -a 7 -b 2) -ne 3) { throw "Test case 5 failed" }
}

Test-GetMaxPens