convert_to_decimal_expression() {
    local base=$1
    local num=$2
    local length=${#num}
    local expression=""
    
    for (( i=0; i<length; i++ )); do
        local digit=${num:$i:1}
        local power=$((length - i - 1))
        
        if [ "$digit" -ne 0 ]; then
            if [ -z "$expression" ]; then
                expression="${digit}*${base}^${power}"
            else
                expression="${expression}+${digit}*${base}^${power}"
            fi
        fi
    done
    
    echo "$expression"
}



test_convert_to_decimal_expression() {
[[ $(convert_to_decimal_expression 2 10101) == "1*2^4+1*2^2+1*2^0" ]] || { echo "Test 1 failed"; exit 1; }
[[ $(convert_to_decimal_expression 3 102) == "1*3^2+2*3^0" ]] || { echo "Test 2 failed"; exit 1; }
[[ $(convert_to_decimal_expression 5 143) == "1*5^2+4*5^1+3*5^0" ]] || { echo "Test 3 failed"; exit 1; }
[[ $(convert_to_decimal_expression 8 123) == "1*8^2+2*8^1+3*8^0" ]] || { echo "Test 4 failed"; exit 1; }
[[ $(convert_to_decimal_expression 4 1302) == "1*4^3+3*4^2+2*4^0" ]] || { echo "Test 5 failed"; exit 1; }
}

test_convert_to_decimal_expression