extract_tenth_line() {
    # Check if the file exists
    if [ ! -f "file.txt" ]; then
        echo "Error: file.txt does not exist" >&2
        return 1
    fi
    
    # Extract the 10th line (using sed)
    sed -n '10p' file.txt
}



test_extract_tenth_line() {
# Create a sample file.txt file
for i in {1..20}; do echo "Line $i"; done > file.txt

# Expected output
local expected_output="Line 10"

# Actual output from the function
local actual_output=$(extract_tenth_line)

# Compare actual output to expected output
[[ "$actual_output" == "$expected_output" ]] || { echo "Test 1 failed"; exit 1; }

# Clean up
rm file.txt
}

# Call the test function
test_extract_tenth_line