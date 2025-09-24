function Convert-IntToBoolAndBack {
    param (
        [int]$intValue
    )
    
    # Convert integer to boolean (non-zero becomes $true, zero becomes $false)
    $boolValue = [bool]$intValue
    
    # Convert boolean back to integer ($true becomes 1, $false becomes 0)
    $result = if ($boolValue) { 1 } else { 0 }
    
    return $result
}


function Test-ConvertIntToBoolAndBack {
    if ((Convert-IntToBoolAndBack -intValue 3) -ne 1) { throw "Test case 1 failed" }
    if ((Convert-IntToBoolAndBack -intValue 0) -ne 0) { throw "Test case 2 failed" }
    if ((Convert-IntToBoolAndBack -intValue 1) -ne 1) { throw "Test case 3 failed" }
    if ((Convert-IntToBoolAndBack -intValue -1) -ne 1) { throw "Test case 4 failed" }
    if ((Convert-IntToBoolAndBack -intValue 100) -ne 1) { throw "Test case 5 failed" }
}

Test-ConvertIntToBoolAndBack