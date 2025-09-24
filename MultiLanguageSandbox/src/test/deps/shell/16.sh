count_unique_strings() {
    local -a words1=($1)
    local -a words2=($2)
    
    declare -A freq1 freq2
    
    # Count frequencies in first array
    for word in "${words1[@]}"; do
        ((freq1["$word"]++))
    done
    
    # Count frequencies in second array
    for word in "${words2[@]}"; do
        ((freq2["$word"]++))
    done
    
    local count=0
    
    # Check words in first array that appear exactly once in both
    for word in "${freq1[@]}"; do
        if [[ ${freq1["$word"]} -eq 1 && ${freq2["$word"]} -eq 1 ]]; then
            ((count++))
        fi
    done
    
    # Check words in second array that appear exactly once in both (not already counted)
    for word in "${freq2[@]}"; do
        if [[ ${freq2["$word"]} -eq 1 && ${freq1["$word"]} -eq 1 ]]; then
            # Only count if not already counted in first pass
            if [[ -z ${freq1["$word"]} || ${freq1["$word"]} -ne 1 ]]; then
                continue
            fi
            ((count++))
        fi
    done
    
    echo $count
}



# Testing function
test_count_unique_strings() {
[[ $(count_unique_strings "apple banana mango" "banana fruits apple") == "2" ]] || { echo "Test 1 failed"; exit 1; }
[[ $(count_unique_strings "hello world" "world hello planet") == "2" ]] || { echo "Test 2 failed"; exit 1; }
[[ $(count_unique_strings "one two three" "four five six") == "0" ]] || { echo "Test 3 failed"; exit 1; }
[[ $(count_unique_strings "one two two" "two one one") == "0" ]] || { echo "Test 4 failed"; exit 1; }
[[ $(count_unique_strings "a b c d e" "a b c d e") == "5" ]] || { echo "Test 5 failed"; exit 1; }
[[ $(count_unique_strings "dog cat" "cat dog bird") == "2" ]] || { echo "Test 6 failed"; exit 1; }
[[ $(count_unique_strings "x y z" "x y z a b c") == "3" ]] || { echo "Test 7 failed"; exit 1; }
}

test_count_unique_strings