previous_permutation() {
    local n=$1
    local permutation=($2)
    local i j k
    
    # Find the largest index i such that permutation[i] > permutation[i+1]
    for ((i = n - 2; i >= 0; i--)); do
        if [[ ${permutation[i]} -gt ${permutation[i+1]} ]]; then
            break
        fi
    done
    
    # If no such index exists, this is the first permutation
    if [[ $i -lt 0 ]]; then
        echo "ERROR"
        return
    fi
    
    # Find the largest index j > i such that permutation[j] < permutation[i]
    for ((j = n - 1; j > i; j--)); do
        if [[ ${permutation[j]} -lt ${permutation[i]} ]]; then
            break
        fi
    done
    
    # Swap permutation[i] and permutation[j]
    k=${permutation[i]}
    permutation[i]=${permutation[j]}
    permutation[j]=$k
    
    # Reverse the suffix starting at i+1
    for ((j = i + 1, k = n - 1; j < k; j++, k--)); do
        k=${permutation[j]}
        permutation[j]=${permutation[k]}
        permutation[k]=$k
    done
    
    # Output the result
    echo "${permutation[@]}"
}



test_previous_permutation() {
[[ $(previous_permutation 3 "1 3 2") == "1 2 3" ]] || { echo "Test 1 failed"; exit 1; }
[[ $(previous_permutation 3 "1 2 3") == "ERROR" ]] || { echo "Test 2 failed"; exit 1; }
[[ $(previous_permutation 4 "2 1 4 3") == "2 1 3 4" ]] || { echo "Test 3 failed"; exit 1; }
[[ $(previous_permutation 5 "5 4 3 2 1") == "5 4 3 1 2" ]] || { echo "Test 4 failed"; exit 1; }
[[ $(previous_permutation 4 "1 4 3 2") == "1 4 2 3" ]] || { echo "Test 5 failed"; exit 1; }
}

test_previous_permutation