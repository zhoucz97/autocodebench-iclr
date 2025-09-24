calculate_expression() {
    local a=$1
    local b=$2
    local c=$3
    echo $(( (a + b) * c ))
}



test_calculate_expression() {
    local result

    result=$(calculate_expression 1 2 3)
    [[ "$result" -eq 9 ]] || { echo "Test 1 failed: Expected 9, got $result"; exit 1; }

    result=$(calculate_expression -1 2 -3)
    [[ "$result" -eq -3 ]] || { echo "Test 2 failed: Expected -3, got $result"; exit 1; }

    result=$(calculate_expression 0 0 1)
    [[ "$result" -eq 0 ]] || { echo "Test 3 failed: Expected 0, got $result"; exit 1; }

    result=$(calculate_expression 10 -5 2)
    [[ "$result" -eq 10 ]] || { echo "Test 4 failed: Expected 10, got $result"; exit 1; }

    result=$(calculate_expression 100 200 -1)
    [[ "$result" -eq -300 ]] || { echo "Test 5 failed: Expected -300, got $result"; exit 1; }
}

test_calculate_expression