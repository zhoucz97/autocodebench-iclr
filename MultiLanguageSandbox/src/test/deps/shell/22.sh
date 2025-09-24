toggle_lights() {
    local n=$1
    local i=1
    while [ $((i * i)) -le $n ]; do
        echo -n "$((i * i)) "
        ((i++))
    done
    echo
}



test_toggle_lights() {
[[ $(toggle_lights 5) == "1 4 " ]] || { echo "Test 1 failed"; exit 1; }
[[ $(toggle_lights 6) == "1 4 " ]] || { echo "Test 2 failed"; exit 1; }
[[ $(toggle_lights 10) == "1 4 9 " ]] || { echo "Test 3 failed"; exit 1; }
[[ $(toggle_lights 3) == "1 " ]] || { echo "Test 4 failed"; exit 1; }
[[ $(toggle_lights 7) == "1 4 " ]] || { echo "Test 5 failed"; exit 1; }
}

test_toggle_lights