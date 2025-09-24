arrange_cards_for_max_number() {
    local cards=($1)
    local count_5=0
    local count_0=0
    
    for card in "${cards[@]}"; do
        if [ "$card" -eq 5 ]; then
            ((count_5++))
        elif [ "$card" -eq 0 ]; then
            ((count_0++))
        fi
    done
    
    # Check if there's at least one 0 and the number of 5s is a multiple of 9
    if [ "$count_0" -lt 1 ] || [ $((count_5 % 9)) -ne 0 ]; then
        echo -1
        return
    fi
    
    # Construct the largest number: as many 5s as possible (multiple of 9) followed by all 0s
    local result=""
    local num_5s_to_use=$(( (count_5 / 9) * 9 ))
    
    for ((i=0; i<num_5s_to_use; i++)); do
        result+="5"
    done
    
    for ((i=0; i<count_0; i++)); do
        result+="0"
    done
    
    echo "$result"
}



test_arrange_cards_for_max_number() {
[[ $(arrange_cards_for_max_number "5 0 5 0") == "0" ]] || { echo "Test 1 failed"; exit 1; }
[[ $(arrange_cards_for_max_number "5 5 5 5 5 5 5 5 0 5 5") == "5555555550" ]] || { echo "Test 2 failed"; exit 1; }
[[ $(arrange_cards_for_max_number "5 5") == "-1" ]] || { echo "Test 3 failed"; exit 1; }
[[ $(arrange_cards_for_max_number "0 0 0") == "0" ]] || { echo "Test 4 failed"; exit 1; }
[[ $(arrange_cards_for_max_number "5 5 5 5 5 5 5 5 5 0") == "5555555550" ]] || { echo "Test 5 failed"; exit 1; }
}

test_arrange_cards_for_max_number