function Transpose-File {
    param (
        [string]$Path
    )

    # Read all lines from the file
    $lines = Get-Content -Path $Path

    # Split each line into an array of elements
    $matrix = $lines | ForEach-Object { $_ -split '\s+' }

    # Determine the number of columns (assuming all rows have same length)
    $columnCount = ($matrix[0]).Count

    # Transpose the matrix
    for ($i = 0; $i -lt $columnCount; $i++) {
        $transposedLine = $matrix | ForEach-Object { $_[ $i ] }
        $transposedLine -join ' '
    }
}


function Test-TransposeFile {
# Test function for Transpose-File
# Create a sample file.txt file
"1 2 3", "4 5 6", "7 8 9" | Set-Content file.txt

# Expected output
$expected_output = "1 4 7", "2 5 8", "3 6 9"

# Actual output from the function
$actual_output = Transpose-File

# Compare actual output to expected output
if (($actual_output -join "`n") -eq ($expected_output -join "`n")) {
"Test 1 passed"
} else {
Write-Error "Test 1 failed"
exit 1
}

# Clean up
Remove-Item file.txt
}

# Call the test function
Test-TransposeFile