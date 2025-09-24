function Find-MinimumSeriesSum {
    param (
        [int]$k
    )

    $sum = 0.0
    $n = 0

    while ($sum -le $k) {
        $n++
        $sum += 1.0 / $n
    }

    return $n
}


function Test-FindMinimumSeriesSum {
    if ((Find-MinimumSeriesSum -k 2) -ne 4) { throw "Test case 1 failed" }
    if ((Find-MinimumSeriesSum -k 3) -ne 11) { throw "Test case 2 failed" }
    if ((Find-MinimumSeriesSum -k 4) -ne 31) { throw "Test case 3 failed" }
    if ((Find-MinimumSeriesSum -k 5) -ne 83) { throw "Test case 4 failed" }
    if ((Find-MinimumSeriesSum -k 6) -ne 227) { throw "Test case 5 failed" }
}

Test-FindMinimumSeriesSum