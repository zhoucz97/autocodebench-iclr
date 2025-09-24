calculate_sine() {
    local a=$1
    local b=$2
    local c=$3
    
    # Find the hypotenuse (the largest number)
    local hypotenuse=$c
    if [ $a -gt $hypotenuse ]; then
        hypotenuse=$a
    fi
    if [ $b -gt $hypotenuse ]; then
        hypotenuse=$b
    fi
    
    # The other two sides are the legs
    local leg1
    local leg2
    if [ $a -eq $hypotenuse ]; then
        leg1=$b
        leg2=$c
    elif [ $b -eq $hypotenuse ]; then
        leg1=$a
        leg2=$c
    else
        leg1=$a
        leg2=$b
    fi
    
    # Determine which leg is opposite the smaller acute angle
    # The smaller leg is opposite the smaller angle
    local opposite
    if [ $leg1 -lt $leg2 ]; then
        opposite=$leg1
    else
        opposite=$leg2
    fi
    
    # Output as reduced fraction (since inputs are Pythagorean triplets, they're already reduced)
    echo "$opposite/$hypotenuse"
}



test_calculate_sine() {
[[ $(calculate_sine 3 5 4) == "3/5" ]] || { echo "Test 1 failed"; exit 1; }
[[ $(calculate_sine 5 12 13) == "5/13" ]] || { echo "Test 2 failed"; exit 1; }
[[ $(calculate_sine 8 15 17) == "8/17" ]] || { echo "Test 3 failed"; exit 1; }
[[ $(calculate_sine 7 24 25) == "7/25" ]] || { echo "Test 4 failed"; exit 1; }
[[ $(calculate_sine 9 40 41) == "9/41" ]] || { echo "Test 5 failed"; exit 1; }
}

test_calculate_sine