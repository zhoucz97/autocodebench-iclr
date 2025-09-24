find_valid_numbers() { grep -E '^([0-9]{3}-[0-9]{3}-[0-9]{4}|(\([0-9]{3}\)[[:space:]]?[0-9]{3}-[0-9]{4})|[0-9]{3}[[:space:]][0-9]{3}[[:space:]][0-9]{4}|(\([0-9]{3}\)-[0-9]{3}-[0-9]{4}))$' file.txt; }



test_find_valid_numbers() {
# Create a sample file.txt file
echo -e "123-456-7890\n(123) 456-7890\n123 456 7890\n(123)-456-7890" > file.txt

# Expected output
local expected_output=$(echo -e "123-456-7890\n(123) 456-7890")

# Actual output from the function
local actual_output=$(find_valid_numbers)

# Compare actual output to expected output
[[ "$actual_output" == "$expected_output" ]] || { echo "Test 1 failed"; exit 1; }

# Clean up
rm file.txt
}

# Call the test function
test_find_valid_numbers