count_digit_occurrences() {
    local n=$1
    local x=$2
    local count=0

    for ((i=1; i<=n; i++)); do
        num=$i
        while [ $num -gt 0 ]; do
            digit=$((num % 10))
            if [ $digit -eq $x ]; then
                count=$((count + 1))
            fi
            num=$((num / 10))
        done
    done

    echo $count
}



test_count_digit_occurrences() {
[[ $(count_digit_occurrences 11 1) -eq 4 ]] || { echo "Test 1 failed"; exit 1; }
[[ $(count_digit_occurrences 20 2) -eq 3 ]] || { echo "Test 2 failed"; exit 1; }
[[ $(count_digit_occurrences 100 3) -eq 20 ]] || { echo "Test 3 failed"; exit 1; }
[[ $(count_digit_occurrences 50 4) -eq 15 ]] || { echo "Test 4 failed"; exit 1; }
[[ $(count_digit_occurrences 99 5) -eq 20 ]] || { echo "Test 5 failed"; exit 1; }
}

test_count_digit_occurrences