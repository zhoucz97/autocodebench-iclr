print_ascii_code() {
    # Check if exactly one argument is provided
    if [ "$#" -ne 1 ]; then
        echo "Usage: print_ascii_code <single_character>"
        return 1
    fi
    
    local char="$1"
    
    # Check if the input is a single character (excluding space)
    if [ "${#char}" -ne 1 ] || [ "$char" = " " ]; then
        echo "Error: Input must be a single visible character (excluding space)"
        return 1
    fi
    
    # Get ASCII code using printf
    printf "%d\n" "'$char"
}



test_print_ascii_code() {
    local result

    result=$(print_ascii_code 'B')
    [[ "$result" -eq 66 ]] || { echo "Test 1 failed: Expected 66, got $result"; exit 1; }

    result=$(print_ascii_code 'z')
    [[ "$result" -eq 122 ]] || { echo "Test 2 failed: Expected 122, got $result"; exit 1; }

    result=$(print_ascii_code '1')
    [[ "$result" -eq 49 ]] || { echo "Test 3 failed: Expected 49, got $result"; exit 1; }

    result=$(print_ascii_code '%')
    [[ "$result" -eq 37 ]] || { echo "Test 4 failed: Expected 37, got $result"; exit 1; }

    result=$(print_ascii_code '?')
    [[ "$result" -eq 63 ]] || { echo "Test 5 failed: Expected 63, got $result"; exit 1; }
}

test_print_ascii_code