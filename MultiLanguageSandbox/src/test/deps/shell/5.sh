process_array() {
    local nums=($1)
    declare -A count
    local pairs=0
    local remaining=0

    # Count occurrences of each number
    for num in "${nums[@]}"; do
        ((count[$num]++))
    done

    # Calculate pairs and remaining
    for num in "${count[@]}"; do
        pairs=$((pairs + count[$num] / 2))
        remaining=$((remaining + count[$num] % 2))
    done

    echo "$pairs $remaining"
}



# Testing function
test_process_array() {
[[ $(process_array "3 1 2 3 2") == "2 1" ]] || { echo "Test 1 failed"; exit 1; }
[[ $(process_array "1 2 3 4") == "0 4" ]] || { echo "Test 2 failed"; exit 1; }
[[ $(process_array "5 5 5 5 5") == "2 1" ]] || { echo "Test 3 failed"; exit 1; }
[[ $(process_array "1 1 2 2 3 3 4 4") == "4 0" ]] || { echo "Test 4 failed"; exit 1; }
[[ $(process_array "1") == "0 1" ]] || { echo "Test 5 failed"; exit 1; }
[[ $(process_array "") == "0 0" ]] || { echo "Test 6 failed"; exit 1; }
[[ $(process_array "2 2 2 2 3 3 3") == "3 1" ]] || { echo "Test 7 failed"; exit 1; }
}

test_process_array