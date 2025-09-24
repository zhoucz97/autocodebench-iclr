int_to_bool_to_int() {
    local initial_int=$1
    # Convert integer to boolean (0 becomes 0, anything else becomes 1)
    local bool_value=$((initial_int != 0))
    # Convert boolean back to integer (which is already 0 or 1)
    local final_int=$bool_value
    echo $final_int
}



test_int_to_bool_to_int() {
    local result

    result=$(int_to_bool_to_int 0)
    [[ "$result" -eq 0 ]] || { echo "Test 1 failed: Expected 0, got $result"; exit 1; }

    result=$(int_to_bool_to_int -5)
    [[ "$result" -eq 1 ]] || { echo "Test 2 failed: Expected 1, got $result"; exit 1; }

    result=$(int_to_bool_to_int 10)
    [[ "$result" -eq 1 ]] || { echo "Test 3 failed: Expected 1, got $result"; exit 1; }

    result=$(int_to_bool_to_int -1)
    [[ "$result" -eq 1 ]] || { echo "Test 4 failed: Expected 1, got $result"; exit 1; }

    result=$(int_to_bool_to_int 1)
    [[ "$result" -eq 1 ]] || { echo "Test 5 failed: Expected 1, got $result"; exit 1; }
}

test_int_to_bool_to_int