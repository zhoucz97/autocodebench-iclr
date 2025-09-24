find_sum_segments() {
    local M=$1
    local result=()
    
    # Iterate possible starting points
    for ((start=1; start<=M/2; start++)); do
        local sum=0
        for ((end=start; end<=M; end++)); do
            sum=$((sum + end))
            if [ $sum -eq $M ]; then
                if [ $((end - start)) -ge 1 ]; then  # Ensure at least two numbers
                    result+=("$start $end")
                fi
                break
            elif [ $sum -gt $M ]; then
                break
            fi
        done
    done
    
    # Print results sorted by starting number
    printf "%s\n" "${result[@]}" | sort -n -k1
}



test_find_sum_segments() {
local result=$(find_sum_segments 10000)
local expected="18 142
297 328
388 412
1998 2002"
[[ $result == "$expected" ]] || { echo "Test 1 failed"; exit 1; }

result=$(find_sum_segments 100)
expected="9 16
18 22"
[[ $result == "$expected" ]] || { echo "Test 2 failed"; exit 1; }
}

test_find_sum_segments