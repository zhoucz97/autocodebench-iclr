find_target_positions() {
    local -a nums=($1)
    local target=$2
    local n=${#nums[@]}
    local left=0
    local right=$((n - 1))
    local start=-1
    local end=-1

    # Binary search to find the first occurrence
    while [ $left -le $right ]; do
        local mid=$(((left + right) / 2))
        if [ ${nums[$mid]} -lt $target ]; then
            left=$((mid + 1))
        else
            right=$((mid - 1))
        fi
    done
    if [ $left -lt $n ] && [ ${nums[$left]} -eq $target ]; then
        start=$left
    else
        echo "-1 -1"
        return
    fi

    # Binary search to find the last occurrence
    left=0
    right=$((n - 1))
    while [ $left -le $right ]; do
        local mid=$(((left + right) / 2))
        if [ ${nums[$mid]} -le $target ]; then
            left=$((mid + 1))
        else
            right=$((mid - 1))
        fi
    done
    end=$((right))

    echo "$start $end"
}



test_find_target_positions() {
[[ $(find_target_positions "5 7 7 8 8 10" 8) == "3 4" ]] || { echo "Test 1 failed"; exit 1; }
[[ $(find_target_positions "5 7 7 8 8 10" 6) == "-1 -1" ]] || { echo "Test 2 failed"; exit 1; }
[[ $(find_target_positions "1 2 3 4 5" 5) == "4 4" ]] || { echo "Test 3 failed"; exit 1; }
[[ $(find_target_positions "1 2 3 4 5" 1) == "0 0" ]] || { echo "Test 4 failed"; exit 1; }
[[ $(find_target_positions "1 1 1 1 1" 1) == "0 4" ]] || { echo "Test 5 failed"; exit 1; }
[[ $(find_target_positions "1 2 3 3 3 4 5" 3) == "2 4" ]] || { echo "Test 6 failed"; exit 1; }
[[ $(find_target_positions "1 2 2 3 4 5" 2) == "1 2" ]] || { echo "Test 7 failed"; exit 1; }
}

test_find_target_positions