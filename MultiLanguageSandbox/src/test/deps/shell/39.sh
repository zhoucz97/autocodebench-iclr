check_lottery_winnings() {
    local winning_numbers=($1)
    shift
    local tickets=("$@")
    
    # Initialize counts for each prize category (0-6 matches)
    local counts=(0 0 0 0 0 0 0)
    
    for ticket in "${tickets[@]}"; do
        local ticket_numbers=($ticket)
        local matches=0
        
        # Count how many numbers match between ticket and winning numbers
        for num in "${winning_numbers[@]}"; do
            if [[ " ${ticket_numbers[*]} " == *" $num "* ]]; then
                ((matches++))
            fi
        done
        
        # Update the corresponding count (index is matches-1, since 0 is index 0)
        if (( matches >= 1 && matches <= 7 )); then
            ((counts[matches-1]++))
        fi
    done
    
    # Return the counts as space-separated values
    echo "${counts[*]}"
}



test_check_lottery_winnings() {
local result
result=$(check_lottery_winnings "23 31 1 14 19 17 18" "12 8 9 23 1 16 7" "11 7 10 21 2 9 31")
[[ $result == "0 0 0 0 0 1 1" ]] || { echo "Test 1 failed"; exit 1; }

result=$(check_lottery_winnings "5 6 7 8 9 10 11" "1 2 3 4 5 6 7" "5 6 7 12 13 14 15")
[[ $result == "0 0 0 0 2 0 0" ]] || { echo "Test 2 failed"; exit 1; }

result=$(check_lottery_winnings "15 16 17 18 19 20 21" "22 23 24 25 26 27 28" "29 30 31 32 33 1 2")
[[ $result == "0 0 0 0 0 0 0" ]] || { echo "Test 3 failed"; exit 1; }
}

test_check_lottery_winnings