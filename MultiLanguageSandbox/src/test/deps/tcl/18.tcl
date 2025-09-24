proc count_unique_strings {words1 words2} {
    # Create frequency dictionaries for both word lists
    array set freq1 {}
    foreach word $words1 {
        if {[info exists freq1($word)]} {
            incr freq1($word)
        } else {
            set freq1($word) 1
        }
    }
    
    array set freq2 {}
    foreach word $words2 {
        if {[info exists freq2($word)]} {
            incr freq2($word)
        } else {
            set freq2($word) 1
        }
    }
    
    # Find words that appear exactly once in both arrays
    set count 0
    foreach word [array names freq1] {
        if {$freq1($word) == 1 && [info exists freq2($word)] && $freq2($word) == 1} {
            incr count
        }
    }
    
    return $count
}


proc check {count_unique_strings} {
if {[count_unique_strings {"apple" "banana" "cherry"} {"banana" "kiwi" "apple"}] == 2} {
puts "Test case 1 Passed"
} else {
error "Test case 1 Failed"
}

if {[count_unique_strings {"a" "b" "c"} {"c" "b" "d"}] == 2} {
puts "Test case 2 Passed"
} else {
error "Test case 2 Failed"
}

if {[count_unique_strings {"one" "two" "three"} {"four" "five" "six"}] == 0} {
puts "Test case 3 Passed"
} else {
error "Test case 3 Failed"
}

if {[count_unique_strings {"alpha" "beta" "gamma"} {"alpha" "beta" "gamma"}] == 3} {
puts "Test case 4 Passed"
} else {
error "Test case 4 Failed"
}

if {[count_unique_strings {"x" "y" "z"} {"a" "b" "z"}] == 1} {
puts "Test case 5 Passed"
} else {
error "Test case 5 Failed"
}

if {[count_unique_strings {"cat" "dog" "cat"} {"dog" "dog" "cat"}] == 0} {
puts "Test case 6 Passed"
} else {
error "Test case 6 Failed"
}

if {[count_unique_strings {"hello" "world"} {"world" "hello"}] == 2} {
puts "Test case 7 Passed"
} else {
error "Test case 7 Failed"
}
}

# Call the check procedure
check count_unique_strings