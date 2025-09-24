proc is_prefix_string {s words} {
    set concatenated ""
    foreach word $words {
        append concatenated $word
        if {$concatenated eq $s} {
            return true
        }
        if {[string length $concatenated] > [string length $s]} {
            return false
        }
    }
    return false
}


proc check {is_prefix_string} {
if {[is_prefix_string "iloveleetcode" {"i" "love" "leetcode" "apples"}] == 1} {
puts "Test case 1 Passed"
} else {
error "Test case 1 Failed"
}

if {[is_prefix_string "iloveleetcodex" {"i" "love" "leetcode" "apples"}] == 0} {
puts "Test case 2 Passed"
} else {
error "Test case 2 Failed"
}

if {[is_prefix_string "hello" {"hello" "world"}] == 1} {
puts "Test case 3 Passed"
} else {
error "Test case 3 Failed"
}

if {[is_prefix_string "helloworld" {"hello" "world"}] == 1} {
puts "Test case 4 Passed"
} else {
error "Test case 4 Failed"
}

if {[is_prefix_string "hell" {"hello" "world"}] == 0} {
puts "Test case 5 Passed"
} else {
error "Test case 5 Failed"
}

if {[is_prefix_string "" {"a" "b" "c"}] == 0} {
puts "Test case 6 Passed"
} else {
error "Test case 6 Failed"
}

if {[is_prefix_string "abc" {"a" "b" "c"}] == 1} {
puts "Test case 7 Passed"
} else {
error "Test case 7 Failed"
}
}

# Call the check procedure
check is_prefix_string