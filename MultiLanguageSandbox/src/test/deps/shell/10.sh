generate_max_local() {
    local -a grid
    IFS=';' read -ra grid <<< "$1"
    local n=${#grid[@]}
    local result=()
    
    for ((i=0; i<n-2; i++)); do
        local row_result=()
        for ((j=0; j<n-2; j++)); do
            # Extract the 3x3 submatrix starting at (i,j)
            local max_val=0
            for ((di=0; di<3; di++)); do
                local row=(${grid[i+di]})
                for ((dj=0; dj<3; dj++)); do
                    if (( row[j+dj] > max_val )); then
                        max_val=${row[j+dj]}
                    fi
                done
            done
            row_result+=("$max_val")
        done
        result+=("$(IFS=' '; echo "${row_result[*]}")")
    done
    
    echo "$(IFS=';'; echo "${result[*]}")"
}



# Testing function
test_generate_max_local() {
[[ $(generate_max_local "1 2 3 4;5 6 7 8;9 10 11 12;13 14 15 16") == "11 12 15 16" ]] || { echo "Test 1 failed"; exit 1; }
[[ $(generate_max_local "9 1 7;4 8 2;3 6 5") == "9" ]] || { echo "Test 2 failed"; exit 1; }
[[ $(generate_max_local "1 2 3;4 5 6;7 8 9") == "9" ]] || { echo "Test 3 failed"; exit 1; }
[[ $(generate_max_local "5 6 7;8 9 10;11 12 13") == "13" ]] || { echo "Test 4 failed"; exit 1; }
[[ $(generate_max_local "10 20 30;40 50 60;70 80 90") == "90" ]] || { echo "Test 5 failed"; exit 1; }
}

test_generate_max_local