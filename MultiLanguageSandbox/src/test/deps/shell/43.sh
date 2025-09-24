ascii_to_char() {
    # Check if an argument is provided
    if [ -z "$1" ]; then
        echo "Error: No ASCII code provided"
        return 1
    fi

    # Convert the ASCII code to a character
    printf "\\$(printf '%03o' "$1")"
    echo  # Print a newline for better readability
}



test_ascii_to_char() {
    local result

    result=$(ascii_to_char 66)
    [[ "$result" == "B" ]] || { echo "Test 1 failed: Expected B, got $result"; exit 1; }

    result=$(ascii_to_char 97)
    [[ "$result" == "a" ]] || { echo "Test 2 failed: Expected a, got $result"; exit 1; }

    result=$(ascii_to_char 36)
    [[ "$result" == "$" ]] || { echo "Test 3 failed: Expected $, got $result"; exit 1; }

    result=$(ascii_to_char 57)
    [[ "$result" == "9" ]] || { echo "Test 4 failed: Expected 9, got $result"; exit 1; }

    result=$(ascii_to_char 32)
    [[ "$result" == " " ]] || { echo "Test 5 failed: Expected space, got $result"; exit 1; }
}

test_ascii_to_char