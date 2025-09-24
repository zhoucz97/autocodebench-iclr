multiply_numbers() {
    # Check if exactly two arguments are provided
    if [ "$#" -ne 2 ]; then
        echo "Usage: multiply_numbers A B"
        return 1
    fi

    # Assign arguments to variables
    local A="$1"
    local B="$2"

    # Check if both arguments are positive integers
    if ! [[ "$A" =~ ^[1-9][0-9]*$ ]] || ! [[ "$B" =~ ^[1-9][0-9]*$ ]]; then
        echo "Error: Both arguments must be positive integers"
        return 1
    fi

    # Calculate the product
    local product=$((A * B))

    # Check for arithmetic overflow (in bash, this would typically be 2^64-1)
    # Note: Bash doesn't have built-in overflow checking, so we'll use a simple check
    if [ "$product" -lt 0 ] || [ "$product" -gt $((2**64-1)) ]; then
        echo "Error: Product exceeds shell arithmetic limits"
        return 1
    fi

    # Print the result
    echo "$product"
}



test_multiply_numbers() {
    local result

    result=$(multiply_numbers 5 10)
    [[ "$result" -eq 50 ]] || { echo "Test 1 failed: Expected 50, got $result"; exit 1; }

    result=$(multiply_numbers 123 456)
    [[ "$result" -eq 56088 ]] || { echo "Test 2 failed: Expected 56088, got $result"; exit 1; }

    result=$(multiply_numbers 1 50000)
    [[ "$result" -eq 50000 ]] || { echo "Test 3 failed: Expected 50000, got $result"; exit 1; }

    result=$(multiply_numbers 250 200)
    [[ "$result" -eq 50000 ]] || { echo "Test 4 failed: Expected 50000, got $result"; exit 1; }

    result=$(multiply_numbers 500 100)
    [[ "$result" -eq 50000 ]] || { echo "Test 5 failed: Expected 50000, got $result"; exit 1; }
}

test_multiply_numbers