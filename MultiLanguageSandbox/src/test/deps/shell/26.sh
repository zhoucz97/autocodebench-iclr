generate_password() {
    local shift=$1
    local string=$2
    local result=""
    
    for (( i=0; i<${#string}; i++ )); do
        char=${string:$i:1}
        
        if [[ $char =~ [a-z] ]]; then
            # Calculate new character position (a=97, z=122)
            ascii=$(printf "%d" "'$char")
            new_ascii=$(( (ascii - 97 + shift) % 26 + 97 ))
            new_char=$(printf "\\$(printf '%03o' "$new_ascii")")
            result+="$new_char"
        elif [[ $char =~ [A-Z] ]]; then
            # Calculate new character position (A=65, Z=90)
            ascii=$(printf "%d" "'$char")
            new_ascii=$(( (ascii - 65 + shift) % 26 + 65 ))
            new_char=$(printf "\\$(printf '%03o' "$new_ascii")")
            result+="$new_char"
        else
            # Leave non-alphabetic characters unchanged
            result+="$char"
        fi
    done
    
    echo "$result"
}



test_generate_password() {
[[ $(generate_password 1 "qwe") == "rxf" ]] || { echo "Test 1 failed"; exit 1; }
[[ $(generate_password 3 "abc") == "def" ]] || { echo "Test 2 failed"; exit 1; }
[[ $(generate_password 2 "xyz") == "zab" ]] || { echo "Test 3 failed"; exit 1; }
[[ $(generate_password 4 "hello") == "lipps" ]] || { echo "Test 4 failed"; exit 1; }
[[ $(generate_password 5 "password") == "ufxxbtwi" ]] || { echo "Test 5 failed"; exit 1; }
}

test_generate_password