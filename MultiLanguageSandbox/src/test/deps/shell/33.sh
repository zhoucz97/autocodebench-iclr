is_lucky_word() {
    local word=$1
    local -A letter_counts
    local max_count=0
    local min_count=999  # Initialize with a large number
    
    # Count the frequency of each letter in the word
    for (( i=0; i<${#word}; i++ )); do
        char=${word:$i:1}
        ((letter_counts[$char]++))
    done
    
    # Find the maximum and minimum counts
    for count in "${letter_counts[@]}"; do
        if (( count > max_count )); then
            max_count=$count
        fi
        if (( count < min_count )); then
            min_count=$count
        fi
    done
    
    # Calculate the difference
    local diff=$((max_count - min_count))
    
    # Check if the difference is a prime number
    if (( diff <= 1 )); then
        echo "No Answer"
        echo 0
        return
    fi
    
    local is_prime=1
    for (( i=2; i*i<=diff; i++ )); do
        if (( diff % i == 0 )); then
            is_prime=0
            break
        fi
    done
    
    if (( is_prime == 1 )); then
        echo "Lucky Word"
        echo $diff
    else
        echo "No Answer"
        echo 0
    fi
}



test_is_lucky_word() {
local result
result=$(is_lucky_word "error")
[[ $result == $'Lucky Word\n2' ]] || { echo "Test 1 failed"; exit 1; }
result=$(is_lucky_word "olympic")
[[ $result == $'No Answer\n0' ]] || { echo "Test 2 failed"; exit 1; }
result=$(is_lucky_word "apple")
[[ $result == $'No Answer\n0' ]] || { echo "Test 3 failed"; exit 1; }
}

test_is_lucky_word