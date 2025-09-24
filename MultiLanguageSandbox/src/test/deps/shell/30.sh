calculate_lexicographical_rank() {
    local N=$1
    local permutation=$2
    local rank=1
    local used=()
    
    # Initialize used array to keep track of used numbers
    for ((i=1; i<=N; i++)); do
        used[$i]=0
    done
    
    for ((i=0; i<N; i++)); do
        local current_digit=${permutation:$i:1}
        local count=0
        
        # Count numbers smaller than current_digit that haven't been used
        for ((num=1; num<current_digit; num++)); do
            if [[ ${used[$num]} -eq 0 ]]; then
                ((count++))
            fi
        done
        
        # Calculate factorial of remaining positions (N - i - 1)
        local fact=1
        for ((j=1; j<=(N - i - 1); j++)); do
            fact=$((fact * j))
        done
        
        rank=$((rank + count * fact))
        
        # Mark current_digit as used
        used[$current_digit]=1
    done
    
    echo $rank
}



test_calculate_lexicographical_rank() {
[[ $(calculate_lexicographical_rank 3 "231") -eq 4 ]] || { echo "Test 1 failed"; exit 1; }
[[ $(calculate_lexicographical_rank 4 "2143") -eq 8 ]] || { echo "Test 2 failed"; exit 1; }
[[ $(calculate_lexicographical_rank 5 "31245") -eq 49 ]] || { echo "Test 3 failed"; exit 1; }
[[ $(calculate_lexicographical_rank 3 "123") -eq 1 ]] || { echo "Test 4 failed"; exit 1; }
[[ $(calculate_lexicographical_rank 4 "4321") -eq 24 ]] || { echo "Test 5 failed"; exit 1; }
}

test_calculate_lexicographical_rank