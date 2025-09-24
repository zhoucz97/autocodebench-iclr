absolute_value() {
    local num=$1
    if [ $num -lt 0 ]; then
        echo $(( -$num ))
    else
        echo $num
    fi
}



test_absolute_value() {
    [[ $(absolute_value 95) -eq 95 ]] || { echo "Test 1 failed"; exit 1; }
    [[ $(absolute_value -95) -eq 95 ]] || { echo "Test 2 failed"; exit 1; }
    [[ $(absolute_value 0) -eq 0 ]] || { echo "Test 3 failed"; exit 1; }
    [[ $(absolute_value 10000) -eq 10000 ]] || { echo "Test 4 failed"; exit 1; }
    [[ $(absolute_value -10000) -eq 10000 ]] || { echo "Test 5 failed"; exit 1; }
    [[ $(absolute_value 1) -eq 1 ]] || { echo "Test 6 failed"; exit 1; }
    [[ $(absolute_value -1) -eq 1 ]] || { echo "Test 7 failed"; exit 1; }
}

test_absolute_value