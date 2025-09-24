calculate_minimum_cost() {
    local n=$1
    local -a pack1=($2)
    local -a pack2=($3)
    local -a pack3=($4)
    
    local q1=${pack1[0]}
    local p1=${pack1[1]}
    local q2=${pack2[0]}
    local p2=${pack2[1]}
    local q3=${pack3[0]}
    local p3=${pack3[1]}
    
    local min_cost=$(( 1 << 30 ))  # Initialize with a large number
    
    # Iterate through possible counts of each package
    # The maximum count for each package is (n / quantity) + 1 to cover possible overages
    local max_count1=$(( (n / q1) + 2 ))
    local max_count2=$(( (n / q2) + 2 ))
    local max_count3=$(( (n / q3) + 2 ))
    
    for (( count1=0; count1<=max_count1; count1++ )); do
        for (( count2=0; count2<=max_count2; count2++ )); do
            for (( count3=0; count3<=max_count3; count3++ )); do
                local total_pencils=$(( count1 * q1 + count2 * q2 + count3 * q3 ))
                if [ $total_pencils -ge $n ]; then
                    local total_cost=$(( count1 * p1 + count2 * p2 + count3 * p3 ))
                    if [ $total_cost -lt $min_cost ]; then
                        min_cost=$total_cost
                    fi
                fi
            done
        done
    done
    
    echo $min_cost
}



test_calculate_minimum_cost() {
[[ $(calculate_minimum_cost 57 "2 2" "50 30" "30 27") -eq 54 ]] || { echo "Test 1 failed"; exit 1; }
[[ $(calculate_minimum_cost 100 "10 5" "20 8" "50 20") -eq 40 ]] || { echo "Test 2 failed"; exit 1; }
[[ $(calculate_minimum_cost 45 "5 3" "15 10" "25 17") -eq 27 ]] || { echo "Test 3 failed"; exit 1; }
[[ $(calculate_minimum_cost 80 "8 4" "40 19" "60 28") -eq 38 ]] || { echo "Test 4 failed"; exit 1; }
[[ $(calculate_minimum_cost 120 "12 6" "24 9" "48 18") -eq 45 ]] || { echo "Test 5 failed"; exit 1; }
}

test_calculate_minimum_cost