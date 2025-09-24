function Has-CloseElements {
    param (
        [Parameter(Mandatory=$true)]
        [System.Collections.Generic.List[float]]$numbers,
        
        [Parameter(Mandatory=$true)]
        [float]$threshold
    )

    # Sort the list to easily find closest pairs
    $sortedNumbers = $numbers | Sort-Object

    # Compare each number with the next one in the sorted list
    for ($i = 0; $i -lt $sortedNumbers.Count - 1; $i++) {
        $current = $sortedNumbers[$i]
        $next = $sortedNumbers[$i + 1]
        
        if ([Math]::Abs($next - $current) -lt $threshold) {
            return $true
        }
    }

    return $false
}


# Test cases
function Check-CloseElements() {
if ((Has-CloseElements (New-Object 'System.Collections.Generic.List[float]' (,([float[]](1.0, 2.0, 3.9, 4.0, 5.0, 2.2)))) 0.3) -ne $true) { throw "Test case 1 failed" }
if ((Has-CloseElements (New-Object 'System.Collections.Generic.List[float]' (,([float[]](1.0, 2.0, 3.9, 4.0, 5.0, 2.2)))) 0.05) -ne $false) { throw "Test case 2 failed" }
if ((Has-CloseElements (New-Object 'System.Collections.Generic.List[float]' (,([float[]](1.0, 2.0, 5.9, 4.0, 5.0)))) 0.95) -ne $true) { throw "Test case 3 failed" }
if ((Has-CloseElements (New-Object 'System.Collections.Generic.List[float]' (,([float[]](1.0, 2.0, 5.9, 4.0, 5.0)))) 0.8) -ne $false) { throw "Test case 4 failed" }
if ((Has-CloseElements (New-Object 'System.Collections.Generic.List[float]' (,([float[]](1.0, 2.0, 3.0, 4.0, 5.0, 2.0)))) 0.1) -ne $true) { throw "Test case 5 failed" }
if ((Has-CloseElements (New-Object 'System.Collections.Generic.List[float]' (,([float[]](1.1, 2.2, 3.1, 4.1, 5.1)))) 1.0) -ne $true) { throw "Test case 6 failed" }
if ((Has-CloseElements (New-Object 'System.Collections.Generic.List[float]' (,([float[]](1.1, 2.2, 3.1, 4.1, 5.1)))) 0.5) -ne $false) { throw "Test case 7 failed" }
}

Check-CloseElements