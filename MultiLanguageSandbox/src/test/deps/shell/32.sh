calculate_min_bridge_rotations() {
    local N=$1
    local carriages=($2)
    local inversions=0

    for ((i = 0; i < N; i++)); do
        for ((j = i + 1; j < N; j++)); do
            if [[ ${carriages[i]} -gt ${carriages[j]} ]]; then
                ((inversions++))
            fi
        done
    done

    echo $inversions
}



test_calculate_min_bridge_rotations() {
[[ $(calculate_min_bridge_rotations 4 "4 3 2 1") -eq 6 ]] || { echo "Test 1 failed"; exit 1; }
[[ $(calculate_min_bridge_rotations 5 "5 4 3 2 1") -eq 10 ]] || { echo "Test 2 failed"; exit 1; }
[[ $(calculate_min_bridge_rotations 3 "3 1 2") -eq 2 ]] || { echo "Test 3 failed"; exit 1; }
[[ $(calculate_min_bridge_rotations 6 "6 5 4 3 2 1") -eq 15 ]] || { echo "Test 4 failed"; exit 1; }
}

test_calculate_min_bridge_rotations