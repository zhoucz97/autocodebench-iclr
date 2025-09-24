merge_items() {
    local items1=(${1//;/ })
    local items2=(${2//;/ })
    declare -A merged
    
    # Process first set of items
    for item in "${items1[@]}"; do
        IFS=' ' read -r value weight <<< "$item"
        merged["$value"]=$(( ${merged["$value"]} + weight ))
    done
    
    # Process second set of items
    for item in "${items2[@]}"; do
        IFS=' ' read -r value weight <<< "$item"
        merged["$value"]=$(( ${merged["$value"]} + weight ))
    done
    
    # Sort and output the results
    for value in $(echo "${merged[@]}" | tr ' ' '\n' | sort -n); do
        echo -n "$value ${merged[$value]};"
    done
    echo
}



test_merge_items() {
[[ $(merge_items "10 5;15 10" "15 15;20 20") == "10 5;15 25;20 20;" ]] || { echo "Test 1 failed"; exit 1; }
[[ $(merge_items "1 2;3 4" "5 6;7 8") == "1 2;3 4;5 6;7 8;" ]] || { echo "Test 2 failed"; exit 1; }
[[ $(merge_items "2 2;4 4" "2 3;4 5") == "2 5;4 9;" ]] || { echo "Test 3 failed"; exit 1; }
[[ $(merge_items "" "") == "" ]] || { echo "Test 4 failed"; exit 1; }
[[ $(merge_items "10 10" "10 10") == "10 20;" ]] || { echo "Test 5 failed"; exit 1; }
[[ $(merge_items "1 1;2 2;3 3" "4 4;5 5;6 6") == "1 1;2 2;3 3;4 4;5 5;6 6;" ]] || { echo "Test 6 failed"; exit 1; }
[[ $(merge_items "9 9;8 8" "7 7;6 6") == "6 6;7 7;8 8;9 9;" ]] || { echo "Test 7 failed"; exit 1; }
}

test_merge_items