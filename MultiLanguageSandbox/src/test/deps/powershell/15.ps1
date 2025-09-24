function Count-WordFrequency {
    # Read the content of the file
    $content = Get-Content -Path "frequency.txt" -Raw
    
    # Split the content into words (split on whitespace)
    $words = $content -split '\s+'
    
    # Create a hashtable to store word frequencies
    $wordFrequency = @{}
    
    # Count the frequency of each word
    foreach ($word in $words) {
        if ($word -ne '') {  # Skip empty strings that might result from splitting
            if ($wordFrequency.ContainsKey($word)) {
                $wordFrequency[$word]++
            } else {
                $wordFrequency[$word] = 1
            }
        }
    }
    
    # Output the results in the format "word count"
    foreach ($key in $wordFrequency.Keys) {
        Write-Output "$key $($wordFrequency[$key])"
    }
}


function Test-CountWordFrequency {
# Test function for Count-WordFrequency
# Create a sample frequency.txt file
"apple banana apple\nbanana banana apple" | Set-Content frequency.txt

# Expected output
$expected_output = "apple 3\nbanana 3" -split "\n"

# Actual output from the function
$actual_output = Count-WordFrequency

# Compare actual output to expected output
if ($actual_output -join "`n" -eq $expected_output -join "`n") {
"Test 1 passed"
} else {
Write-Error "Test 1 failed"
exit 1
}

# Clean up
Remove-Item frequency.txt
}

# Call the test function
Test-CountWordFrequency