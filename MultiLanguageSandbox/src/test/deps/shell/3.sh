calculate_tax() {
    IFS=' ' read -ra brackets <<< "$1"
    local income=$2
    local tax=0
    local prev_upper=0
    
    for ((i=0; i<${#brackets[@]}; i+=2)); do
        local upper=${brackets[i]}
        local percent=${brackets[i+1]}
        
        if [ $income -le $prev_upper ]; then
            break
        fi
        
        local taxable_amount=$(( $upper > $income ? $income - $prev_upper : $upper - $prev_upper ))
        tax=$(( $tax + ($taxable_amount * $percent) / 100 ))
        prev_upper=$upper
    done
    
    echo $tax
}



# Testing function
test_calculate_tax() {
[[ $(calculate_tax "10000 10 20000 20 30000 30" 25000) == "4500" ]] || { echo "Test 1 failed"; exit 1; }
[[ $(calculate_tax "10000 10 20000 20 30000 30" 15000) == "2000" ]] || { echo "Test 2 failed"; exit 1; }
[[ $(calculate_tax "5000 5 10000 10 20000 20" 12000) == "1150" ]] || { echo "Test 3 failed"; exit 1; }
[[ $(calculate_tax "5000 5 10000 10 20000 20" 5000) == "250" ]] || { echo "Test 4 failed"; exit 1; }
[[ $(calculate_tax "10000 10 20000 20" 0) == "0" ]] || { echo "Test 5 failed"; exit 1; }
}

test_calculate_tax