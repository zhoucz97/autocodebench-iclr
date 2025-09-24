has_close_elements() {
    local numbers=($1)
    local threshold=$2
    
    # Sort the numbers to easily find closest pairs
    IFS=$'\n' sorted_numbers=($(sort -n <<<"${numbers[*]}"))
    unset IFS
    
    # Compare each adjacent pair in the sorted list
    for ((i=0; i<${#sorted_numbers[@]}-1; i++)); do
        local diff=$(echo "${sorted_numbers[$((i+1))]} - ${sorted_numbers[$i]}" | bc)
        if (( $(echo "$diff < $threshold" | bc -l) )); then
            echo "True"
            return 0
        fi
    done
    
    echo "False"
    return 1
}



# Testing function
test_close_elements() {
[[ $(has_close_elements "1.0 2.0 3.9 4.0 5.0 2.2" 0.3) == "true" ]] || { echo "Test 1 failed"; exit 1; }
[[ $(has_close_elements "1.0 2.0 3.9 4.0 5.0 2.2" 0.05) == "false" ]] || { echo "Test 2 failed"; exit 1; }
[[ $(has_close_elements "1.0 2.0 5.9 4.0 5.0" 0.95) == "true" ]] || { echo "Test 3 failed"; exit 1; }
[[ $(has_close_elements "1.0 2.0 5.9 4.0 5.0" 0.8) == "false" ]] || { echo "Test 4 failed"; exit 1; }
[[ $(has_close_elements "1.0 2.0 3.0 4.0 5.0 2.0" 0.1) == "true" ]] || { echo "Test 5 failed"; exit 1; }
[[ $(has_close_elements "1.1 2.2 3.1 4.1 5.1" 1.0) == "true" ]] || { echo "Test 6 failed"; exit 1; }
[[ $(has_close_elements "1.1 2.2 3.1 4.1 5.1" 0.5) == "false" ]] || { echo "Test 7 failed"; exit 1; }
}

test_close_elements