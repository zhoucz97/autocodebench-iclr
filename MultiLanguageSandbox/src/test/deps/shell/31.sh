calculate_max_subsequence_sum() {
    local sequence=($1)
    local max_current=${sequence[0]}
    local max_global=${sequence[0]}
    
    for ((i=1; i<${#sequence[@]}; i++)); do
        max_current=$(( ${sequence[i]} > max_current + ${sequence[i]} ? ${sequence[i]} : max_current + ${sequence[i]} ))
        max_global=$(( max_current > max_global ? max_current : max_global ))
    done
    
    echo $max_global
}



test_calculate_max_subsequence_sum() {
    [[ $(calculate_max_subsequence_sum "2 -4 3 -1 2 -4 3") -eq 4 ]] || { echo "Test 1 failed"; exit 1; }
    [[ $(calculate_max_subsequence_sum "-1 -2 -3 -4") -eq -1 ]] || { echo "Test 2 failed"; exit 1; }
    [[ $(calculate_max_subsequence_sum "1 2 3 4 -10 5 6") -eq 11 ]] || { echo "Test 3 failed"; exit 1; }
    [[ $(calculate_max_subsequence_sum "1 2 3 4 5 6") -eq 21 ]] || { echo "Test 4 failed"; exit 1; }
    [[ $(calculate_max_subsequence_sum "-1 -2 -3 -4 -5 -6") -eq -1 ]] || { echo "Test 5 failed"; exit 1; }
}

test_calculate_max_subsequence_sum