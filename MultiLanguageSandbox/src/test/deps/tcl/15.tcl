proc kth_unique_string {arr k} {
    # Create a dictionary to count occurrences of each string
    set count_dict [dict create]
    foreach str $arr {
        dict incr count_dict $str
    }
    
    # Collect strings that appear exactly once
    set unique_strings [list]
    dict for {str count} $count_dict {
        if {$count == 1} {
            lappend unique_strings $str
        }
    }
    
    # Check if there are at least k unique strings
    if {[llength $unique_strings] >= $k} {
        return [lindex $unique_strings [expr {$k - 1}]]
    } else {
        return ""
    }
}


proc check {kth_unique_string} {
if {[kth_unique_string {"a" "b" "a" "c" "b"} 2] eq ""} {
puts "Test case 1 Passed"
} else {
error "Test case 1 Failed"
}

if {[kth_unique_string {"a" "b" "a" "c" "b"} 3] eq ""} {
puts "Test case 2 Passed"
} else {
error "Test case 2 Failed"
}

if {[kth_unique_string {"apple" "banana" "apple" "orange"} 1] eq "banana"} {
puts "Test case 3 Passed"
} else {
error "Test case 3 Failed"
}

if {[kth_unique_string {"hello" "hello" "world"} 1] eq "world"} {
puts "Test case 4 Passed"
} else {
error "Test case 4 Failed"
}

if {[kth_unique_string {"x" "y" "z" "x" "y"} 1] eq "z"} {
puts "Test case 5 Passed"
} else {
error "Test case 5 Failed"
}

if {[kth_unique_string {"one" "two" "three" "four"} 2] eq "two"} {
puts "Test case 6 Passed"
} else {
error "Test case 6 Failed"
}

if {[kth_unique_string {"one" "one" "one"} 1] eq ""} {
puts "Test case 7 Passed"
} else {
error "Test case 7 Failed"
}
}

# Call the check procedure
check kth_unique_string