search_rotated_array() {
    local -a nums=($1)
    local target=$2
    local left=0
    local right=$((${#nums[@]} - 1))
    
    while [ $left -le $right ]; do
        local mid=$(( (left + right) / 2 ))
        
        if [ ${nums[$mid]} -eq $target ]; then
            echo $mid
            return
        fi
        
        # Check if the left half is sorted
        if [ ${nums[$left]} -le ${nums[$mid]} ]; then
            if [ ${nums[$left]} -le $target ] && [ $target -lt ${nums[$mid]} ]; then
                right=$((mid - 1))
            else
                left=$((mid + 1))
            fi
        else
            # Right half must be sorted
            if [ ${nums[$mid]} -lt $target ] && [ $target -le ${nums[$right]} ]; then
                left=$((mid + 1))
            else
                right=$((mid - 1))
            fi
        fi
    done
    
    echo -1
}



test_search_rotated_array() {
[[ $(search_rotated_array "4 5 6 7 0 1 2" 0) == "4" ]] || { echo "Test 1 failed"; exit 1; }
[[ $(search_rotated_array "4 5 6 7 0 1 2" 3) == "-1" ]] || { echo "Test 2 failed"; exit 1; }
[[ $(search_rotated_array "0 1 2 4 5 6 7" 3) == "-1" ]] || { echo "Test 3 failed"; exit 1; }
[[ $(search_rotated_array "6 7 0 1 2 4 5" 3) == "-1" ]] || { echo "Test 4 failed"; exit 1; }
[[ $(search_rotated_array "7 0 1 2 4 5 6" 0) == "1" ]] || { echo "Test 5 failed"; exit 1; }
[[ $(search_rotated_array "1" 0) == "-1" ]] || { echo "Test 6 failed"; exit 1; }
[[ $(search_rotated_array "1 3" 3) == "1" ]] || { echo "Test 7 failed"; exit 1; }
}

test_search_rotated_array