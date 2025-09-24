next_permutation() {
    local -a nums=($1)
    local n=${#nums[@]}
    local i=$((n - 2))
    
    # Step 1: Find the first decreasing element from the end
    while [[ $i -ge 0 && ${nums[$i]} -ge ${nums[$((i + 1))]} ]]; do
        ((i--))
    done
    
    if [[ $i -ge 0 ]]; then
        local j=$((n - 1))
        # Step 2: Find the smallest element greater than nums[i] from the end
        while [[ ${nums[$j]} -le ${nums[$i]} ]]; do
            ((j--))
        done
        # Step 3: Swap nums[i] and nums[j]
        local temp=${nums[$i]}
        nums[$i]=${nums[$j]}
        nums[$j]=$temp
    fi
    
    # Step 4: Reverse the suffix after i
    local left=$((i + 1))
    local right=$((n - 1))
    while [[ $left -lt $right ]]; do
        local temp=${nums[$left]}
        nums[$left]=${nums[$right]}
        nums[$right]=$temp
        ((left++))
        ((right--))
    done
    
    echo "${nums[*]}"
}



test_next_permutation() {
[[ $(next_permutation "1 2 3") == "1 3 2" ]] || { echo "Test 1 failed"; exit 1; }
[[ $(next_permutation "3 2 1") == "1 2 3" ]] || { echo "Test 2 failed"; exit 1; }
[[ $(next_permutation "1 3 2") == "2 1 3" ]] || { echo "Test 3 failed"; exit 1; }
[[ $(next_permutation "2 3 1") == "3 1 2" ]] || { echo "Test 4 failed"; exit 1; }
[[ $(next_permutation "1 1 5") == "1 5 1" ]] || { echo "Test 5 failed"; exit 1; }
[[ $(next_permutation "2 1 3") == "2 3 1" ]] || { echo "Test 6 failed"; exit 1; }
[[ $(next_permutation "5 4 3 2 1") == "1 2 3 4 5" ]] || { echo "Test 7 failed"; exit 1; }
}

test_next_permutation