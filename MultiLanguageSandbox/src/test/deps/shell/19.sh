transpose_file() {
    # Read the input file line by line
    while IFS= read -r line; do
        # Split the line into an array
        arr=($line)
        # If this is the first line, initialize the output arrays
        if [ -z "$first_line" ]; then
            first_line=1
            for ((i=0; i<${#arr[@]}; i++)); do
                output[$i]="${arr[$i]}"
            done
        else
            # For subsequent lines, append to each output array
            for ((i=0; i<${#arr[@]}; i++)); do
                output[$i]="${output[$i]} ${arr[$i]}"
            done
        fi
    done < "file.txt"

    # Print the transposed output
    for ((i=0; i<${#output[@]}; i++)); do
        echo "${output[$i]}"
    done
}



# Test function for transpose_file
test_transpose_file() {
# Create a sample file.txt file
echo -e "1 2 3\n4 5 6\n7 8 9" > file.txt

# Expected output
local expected_output=$(echo -e "1 4 7\n2 5 8\n3 6 9")

# Actual output from the function
local actual_output=$(transpose_file)

# Compare actual output to expected output
[[ "$actual_output" == "$expected_output" ]] || { echo "Test 1 failed"; exit 1; }

# Clean up
rm file.txt
}

# Call the test function
test_transpose_file