find_kth_smallest_unique_number() {
    local numbers=($1)
    local k=$2
    
    # Create an associative array to store unique numbers
    declare -A unique_numbers
    
    # Populate the associative array (keys will be unique)
    for num in "${numbers[@]}"; do
        unique_numbers["$num"]=1
    done
    
    # Extract the keys (unique numbers) and sort them
    sorted_unique=($(printf "%s\n" "${unique_numbers[@]}" | sort -n))
    
    # Check if k is within bounds
    if [ $k -gt ${#sorted_unique[@]} ] || [ $k -le 0 ]; then
        echo "NO RESULT"
    else
        # Bash arrays are 0-indexed, but we want 1-based k
        echo "${sorted_unique[$((k-1))]}"
    fi
}



test_kth_smallest_unique_number() {
[[ $(find_kth_smallest_unique_number "5 1 3 5 2 4 1" 4) == "4" ]] || { echo "Test 1 failed"; exit 1; }
[[ $(find_kth_smallest_unique_number "8 7 6 5 4 3 2 1" 3) == "3" ]] || { echo "Test 2 failed"; exit 1; }
[[ $(find_kth_smallest_unique_number "9 9 8 8 7 7" 2) == "8" ]] || { echo "Test 3 failed"; exit 1; }
[[ $(find_kth_smallest_unique_number "10 20 30 40 50" 5) == "50" ]] || { echo "Test 4 failed"; exit 1; }
[[ $(find_kth_smallest_unique_number "15 15 15 15 15" 1) == "15" ]] || { echo "Test 5 failed"; exit 1; }
[[ $(find_kth_smallest_unique_number "1 2 3 4 5" 6) == "NO RESULT" ]] || { echo "Test 6 failed"; exit 1; }
[[ $(find_kth_smallest_unique_number "3 3 3 3 3" 2) == "NO RESULT" ]] || { echo "Test 7 failed"; exit 1; }
}

test_kth_smallest_unique_number