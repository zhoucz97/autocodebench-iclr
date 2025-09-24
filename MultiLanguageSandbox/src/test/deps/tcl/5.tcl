proc build_concatenated_array {nums} {
    set n [llength $nums]
    set ans {}
    for {set i 0} {$i < $n} {incr i} {
        lappend ans [lindex $nums $i]
    }
    for {set i 0} {$i < $n} {incr i} {
        lappend ans [lindex $nums $i]
    }
    return $ans
}


proc check {build_concatenated_array} {
if {[build_concatenated_array {1 2 3}] eq {1 2 3 1 2 3}} {
puts "Test case 1 Passed"
} else {
error "Test case 1 Failed"
}

if {[build_concatenated_array {4 5 6 7}] eq {4 5 6 7 4 5 6 7}} {
puts "Test case 2 Passed"
} else {
error "Test case 2 Failed"
}

if {[build_concatenated_array {0}] eq {0 0}} {
puts "Test case 3 Passed"
} else {
error "Test case 3 Failed"
}

if {[build_concatenated_array {9 8 7 6 5}] eq {9 8 7 6 5 9 8 7 6 5}} {
puts "Test case 4 Passed"
} else {
error "Test case 4 Failed"
}

if {[build_concatenated_array {1 1 1 1}] eq {1 1 1 1 1 1 1 1}} {
puts "Test case 5 Passed"
} else {
error "Test case 5 Failed"
}

if {[build_concatenated_array {10 20 30}] eq {10 20 30 10 20 30}} {
puts "Test case 6 Passed"
} else {
error "Test case 6 Failed"
}

if {[build_concatenated_array {0 0 0 0 0 0}] eq {0 0 0 0 0 0 0 0 0 0 0 0}} {
puts "Test case 7 Passed"
} else {
error "Test case 7 Failed"
}
}

# Call the check procedure
check build_concatenated_array