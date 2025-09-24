count_meal_combinations() {
    local n=$1
    local m=$2
    local dish_prices=($3)
    local count=0

    # Helper function to perform backtracking
    backtrack() {
        local index=$1
        local remaining=$2
        if [ $remaining -eq 0 ]; then
            ((count++))
            return
        fi
        if [ $index -ge $n ] || [ $remaining -lt 0 ]; then
            return
        fi
        # Include the current dish
        backtrack $((index + 1)) $((remaining - dish_prices[index]))
        # Exclude the current dish
        backtrack $((index + 1)) $remaining
    }

    backtrack 0 $m
    echo $count
}



test_meal_combinations() {
[[ $(count_meal_combinations 3 5 "1 2 3") -eq 1 ]] || { echo "Test 1 failed"; exit 1; }
[[ $(count_meal_combinations 4 6 "1 2 3 1") -eq 2 ]] || { echo "Test 2 failed"; exit 1; }
[[ $(count_meal_combinations 5 10 "2 2 2 2 2") -eq 1 ]] || { echo "Test 3 failed"; exit 1; }
[[ $(count_meal_combinations 3 3 "1 1 1") -eq 1 ]] || { echo "Test 4 failed"; exit 1; }
[[ $(count_meal_combinations 4 8 "1 2 3 5") -eq 2 ]] || { echo "Test 5 failed"; exit 1; }
}

test_meal_combinations