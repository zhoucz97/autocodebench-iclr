evaluate_hand() {
    local ranks=($1)
    local suits=($2)
    
    # Check for Flush (all suits are the same)
    local first_suit=${suits[0]}
    local is_flush=true
    for suit in "${suits[@]}"; do
        if [ "$suit" != "$first_suit" ]; then
            is_flush=false
            break
        fi
    done
    
    if $is_flush; then
        echo "Flush"
        return
    fi
    
    # Check for Three of a Kind
    declare -A rank_counts
    for rank in "${ranks[@]}"; do
        ((rank_counts[$rank]++))
    done
    
    local three_of_a_kind=false
    for count in "${rank_counts[@]}"; do
        if [ "$count" -ge 3 ]; then
            three_of_a_kind=true
            break
        fi
    done
    
    if $three_of_a_kind; then
        echo "Three of a Kind"
        return
    fi
    
    # Check for Pair
    local pair=false
    for count in "${rank_counts[@]}"; do
        if [ "$count" -ge 2 ]; then
            pair=true
            break
        fi
    done
    
    if $pair; then
        echo "Pair"
        return
    fi
    
    # If none of the above, it's High Card
    echo "High Card"
}



# Testing function
test_evaluate_hand() {
[[ $(evaluate_hand "2 3 4 5 6" "H H H H H") == "Flush" ]] || { echo "Test 1 failed"; exit 1; }
[[ $(evaluate_hand "4 4 4 7 8" "H D S C H") == "Three of a Kind" ]] || { echo "Test 2 failed"; exit 1; }
[[ $(evaluate_hand "5 5 9 10 J" "C D H S D") == "Pair" ]] || { echo "Test 3 failed"; exit 1; }
[[ $(evaluate_hand "2 3 5 8 K" "C D H S D") == "High Card" ]] || { echo "Test 4 failed"; exit 1; }
[[ $(evaluate_hand "9 9 9 9 8" "H D S C H") == "Three of a Kind" ]] || { echo "Test 5 failed"; exit 1; }
[[ $(evaluate_hand "A K Q J 10" "H H C D S") == "High Card" ]] || { echo "Test 6 failed"; exit 1; }
[[ $(evaluate_hand "3 3 6 6 K" "H D S C H") == "Pair" ]] || { echo "Test 7 failed"; exit 1; }
}

test_evaluate_hand