min_operations_to_zero() {
    local nums=($1)
    local unique_nums=()
    local seen=()
    
    # Filter out zeros and collect unique non-zero numbers
    for num in "${nums[@]}"; do
        if [[ $num -ne 0 ]]; then
            if [[ ! " ${seen[@]} " =~ " ${num} " ]]; then
                seen+=("$num")
                unique_nums+=("$num")
            fi
        fi
    done
    
    # Sort the unique numbers
    IFS=$'\n' sorted_unique_nums=($(sort -n <<<"${unique_nums[*]}"))
    unset IFS
    
    # The number of operations is the number of unique non-zero elements
    echo "${#sorted_unique_nums[@]}"
}



test_min_operations_to_zero() {
[[ $(min_operations_to_zero "3 3 2 2 1") == "3" ]] || { echo "Test 1 failed"; exit 1; }
[[ $(min_operations_to_zero "4 0 0 4") == "1" ]] || { echo "Test 2 failed"; exit 1; }
[[ $(min_operations_to_zero "1 2 3 4 5") == "5" ]] || { echo "Test 3 failed"; exit 1; }
[[ $(min_operations_to_zero "0 0 0") == "0" ]] || { echo "Test 4 failed"; exit 1; }
[[ $(min_operations_to_zero "5 5 5 5 5") == "1" ]] || { echo "Test 5 failed"; exit 1; }
[[ $(min_operations_to_zero "1 1 1 1") == "1" ]] || { echo "Test 6 failed"; exit 1; }
[[ $(min_operations_to_zero "10 10 10 10 20") == "2" ]] || { echo "Test 7 failed"; exit 1; }
}

test_min_operations_to_zero