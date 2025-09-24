is_x_matrix() {
    IFS=';' read -ra rows <<< "$1"
    local n=${#rows[@]}
    
    for ((i=0; i<n; i++)); do
        IFS=' ' read -ra elements <<< "${rows[i]}"
        for ((j=0; j<${#elements[@]}; j++)); do
            # Check if current element is not on either diagonal
            if [ $i -ne $j ] && [ $((i + j)) -ne $((n - 1)) ]; then
                # If it's not on a diagonal, it should be zero
                if [ "${elements[j]}" -ne 0 ]; then
                    return 1  # false
                fi
            fi
        done
    done
    
    return 0  # true
}



# Testing function
test_is_x_matrix() {
[[ $(is_x_matrix "1 0 0;0 1 0;0 0 1") == "false" ]] || { echo "Test 1 failed"; exit 1; }
[[ $(is_x_matrix "1 0 2;0 1 0;3 0 1") == "true" ]] || { echo "Test 2 failed"; exit 1; }
[[ $(is_x_matrix "2 0 0 0;0 3 0 0;0 0 5 0;0 0 0 7") == "false" ]] || { echo "Test 3 failed"; exit 1; }
[[ $(is_x_matrix "1 0 0;0 0 0;0 0 1") == "false" ]] || { echo "Test 4 failed"; exit 1; }
[[ $(is_x_matrix "0") == "false" ]] || { echo "Test 5 failed"; exit 1; }
[[ $(is_x_matrix "5") == "true" ]] || { echo "Test 6 failed"; exit 1; }
[[ $(is_x_matrix "1 0 1;0 1 0;1 0 1") == "true" ]] || { echo "Test 7 failed"; exit 1; }
}

test_is_x_matrix