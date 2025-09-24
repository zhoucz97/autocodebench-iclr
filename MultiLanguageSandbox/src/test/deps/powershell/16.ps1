function Find-ValidNumbers {
    param (
        [string]$FilePath
    )
    
    $pattern = '^\(?(\d{3})\)?[-.\s]?(\d{3})[-.\s]?(\d{4})$'
    Get-Content $FilePath | Where-Object { $_ -match $pattern }
}


function Test-FindValidNumbers {
# Test function for Find-ValidNumbers
# Create a sample file.txt file
"123-456-7890", "(123) 456-7890", "123 456 7890", "(123)-456-7890" | Set-Content file.txt

# Expected output
$expected_output = "123-456-7890", "(123) 456-7890"

# Actual output from the function
$actual_output = Find-ValidNumbers

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
Test-FindValidNumbers