function Extract-TenthLine {
    # Get all lines from the file
    $lines = Get-Content -Path "file.txt"
    
    # Check if there are at least 10 lines
    if ($lines.Count -ge 10) {
        # Return the tenth line (index 9, since arrays are zero-based)
        return $lines[9]
    } else {
        Write-Warning "The file does not have 10 lines."
        return $null
    }
}


function Test-ExtractTenthLine {
# Test function for Extract-TenthLine
# Create a sample file.txt file
1..20 | ForEach-Object { "Line $_" } | Set-Content file.txt

# Expected output
$expected_output = "Line 10"

# Actual output from the function
$actual_output = Extract-TenthLine

# Compare actual output to expected output
if ($actual_output -eq $expected_output) {
"Test 1 passed"
} else {
Write-Error "Test 1 failed"
exit 1
}

# Clean up
Remove-Item file.txt
}

# Call the test function
Test-ExtractTenthLine