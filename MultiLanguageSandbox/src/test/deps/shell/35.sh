convert_bases() {
    local input_base=$1
    local number=$2
    local target_base=$3

    # Check if bases are within valid range (2-16)
    if ! [[ "$input_base" =~ ^[2-9]$|^1[0-6]$ ]] || ! [[ "$target_base" =~ ^[2-9]$|^1[0-6]$ ]]; then
        echo "INVALID INPUT"
        return
    fi

    # Check if number contains only valid characters for the input base
    case $input_base in
        [2-9])
            if ! [[ "$number" =~ ^[0-9]+$ ]]; then
                echo "INVALID INPUT"
                return
            fi
            ;;
        10)
            if ! [[ "$number" =~ ^[0-9]+$ ]]; then
                echo "INVALID INPUT"
                return
            fi
            ;;
        11|12|13|14|15|16)
            if ! [[ "$number" =~ ^[0-9A-Fa-f]+$ ]]; then
                echo "INVALID INPUT"
                return
            fi
            ;;
    esac

    # Convert the number to decimal first
    local decimal=0
    local len=${#number}
    for (( i=0; i<len; i++ )); do
        local char=${number:$i:1}
        local digit
        case $char in
            [0-9]) digit=$((10#$char)) ;;
            [A-Fa-f]) digit=$((10#$(printf '%d' "'$char") - 55)) ;;
            *) echo "INVALID INPUT"; return ;;
        esac

        if (( digit >= input_base )); then
            echo "INVALID INPUT"
            return
        fi

        decimal=$((decimal * input_base + digit))
    done

    # Now convert from decimal to target base
    if (( target_base == 10 )); then
        echo "$decimal"
        return
    fi

    local result=""
    local temp=$decimal
    while (( temp > 0 )); do
        local remainder=$((temp % target_base))
        local char
        if (( remainder < 10 )); then
            char=$remainder
        else
            char=$(printf "\\$(printf '%03o' $((remainder + 55)))")
        fi
        result="$char$result"
        temp=$((temp / target_base))
    done

    # Handle the case when the number is 0
    if [[ -z "$result" ]]; then
        result="0"
    fi

    echo "$result"
}



test_convert_bases() {
[[ $(convert_bases 10 "1024" 2) == "10000000000" ]] || { echo "Test 1 failed"; exit 1; }
[[ $(convert_bases 16 "1A3" 8) == "643" ]] || { echo "Test 2 failed"; exit 1; }
[[ $(convert_bases 2 "1101" 10) == "13" ]] || { echo "Test 3 failed"; exit 1; }
[[ $(convert_bases 16 "ABC" 2) == "101010111100" ]] || { echo "Test 4 failed"; exit 1; }
[[ $(convert_bases 8 "777" 16) == "1FF" ]] || { echo "Test 5 failed"; exit 1; }
[[ $(convert_bases 4 "123" 10) == "27" ]] || { echo "Test 6 failed"; exit 1; }
[[ $(convert_bases 20 "1A" 2) == "INVALID INPUT" ]] || { echo "Test 7 failed"; exit 1; }
}

test_convert_bases