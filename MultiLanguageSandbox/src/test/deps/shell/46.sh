calculate_nth_term() {
    # Check if the correct number of arguments is provided
    if [ "$#" -ne 3 ]; then
        echo "Usage: calculate_nth_term a1 a2 n"
        return 1
    fi

    # Assign arguments to variables
    local a1=$1
    local a2=$2
    local n=$3

    # Calculate the common difference (d)
    local d=$((a2 - a1))

    # Calculate the nth term using the formula: a_n = a1 + (n-1)*d
    local nth_term=$((a1 + (n - 1) * d))

    # Print the result
    echo "$nth_term"
}



test_calculate_nth_term() {
    local result

    result=$(calculate_nth_term 1 3 5)
    [[ "$result" -eq 9 ]] || { echo "Test 1 failed: Expected 9, got $result"; exit 1; }

    result=$(calculate_nth_term 2 5 10)
    [[ "$result" -eq 29 ]] || { echo "Test 2 failed: Expected 20, got $result"; exit 1; }

    result=$(calculate_nth_term -1 1 50)
    [[ "$result" -eq 97 ]] || { echo "Test 3 failed: Expected 97, got $result"; exit 1; }

    result=$(calculate_nth_term 100 -100 3)
    [[ "$result" -eq -300 ]] || { echo "Test 4 failed: Expected -300, got $result"; exit 1; }

}

test_calculate_nth_term