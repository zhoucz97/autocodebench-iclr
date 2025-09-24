max_water_container() {
    local -a heights=($1)
    local left=0
    local right=$((${#heights[@]} - 1))
    local max_area=0
    local current_height current_width current_area

    while [ $left -lt $right ]; do
        current_height=$((${heights[$left]} < ${heights[$right]} ? ${heights[$left]} : ${heights[$right]}))
        current_width=$(($right - $left))
        current_area=$(($current_height * $current_width))
        
        if [ $current_area -gt $max_area ]; then
            max_area=$current_area
        fi
        
        if [ ${heights[$left]} -lt ${heights[$right]} ]; then
            left=$(($left + 1))
        else
            right=$(($right - 1))
        fi
    done
    
    echo $max_area
}



test_max_water_container() {
[[ $(max_water_container "1 8 6 2 5 4 8 3 7") == "49" ]] || { echo "Test 1 failed"; exit 1; }
[[ $(max_water_container "1 1") == "1" ]] || { echo "Test 2 failed"; exit 1; }
[[ $(max_water_container "4 3 2 1 4") == "16" ]] || { echo "Test 3 failed"; exit 1; }
[[ $(max_water_container "1 2 1") == "2" ]] || { echo "Test 4 failed"; exit 1; }
[[ $(max_water_container "2 3 4 5 18 17 6") == "17" ]] || { echo "Test 5 failed"; exit 1; }
[[ $(max_water_container "1 2 4 3") == "4" ]] || { echo "Test 6 failed"; exit 1; }
[[ $(max_water_container "3 9 3 4 7 2 12 6") == "45" ]] || { echo "Test 7 failed"; exit 1; }
}

test_max_water_container