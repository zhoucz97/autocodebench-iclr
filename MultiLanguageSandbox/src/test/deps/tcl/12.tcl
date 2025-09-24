proc create_2d_array {original m n} {
    # Check if the length of the original array matches m * n
    set len [llength $original]
    if {$len != $m * $n} {
        return {}
    }

    # Initialize the 2D array
    set result {}

    # Populate the 2D array
    for {set i 0} {$i < $m} {incr i} {
        set row {}
        for {set j 0} {$j < $n} {incr j} {
            lappend row [lindex $original [expr {$i * $n + $j}]]
        }
        lappend result $row
    }

    return $result
}


proc check {create_2d_array} {
if {[create_2d_array {1 2 3 4} 2 2] eq {{1 2} {3 4}}} {
puts "Test case 1 Passed"
} else {
error "Test case 1 Failed"
}

if {[create_2d_array {1 2 3} 2 2] eq {}} {
puts "Test case 2 Passed"
} else {
error "Test case 2 Failed"
}

if {[create_2d_array {1 2 3 4 5 6} 2 3] eq {{1 2 3} {4 5 6}}} {
puts "Test case 3 Passed"
} else {
error "Test case 3 Failed"
}

if {[create_2d_array {1 2 3 4 5 6 7 8 9} 3 3] eq {{1 2 3} {4 5 6} {7 8 9}}} {
puts "Test case 4 Passed"
} else {
error "Test case 4 Failed"
}

if {[create_2d_array {1 2 3 4 5 6 7 8} 4 2] eq {{1 2} {3 4} {5 6} {7 8}}} {
puts "Test case 5 Passed"
} else {
error "Test case 5 Failed"
}

if {[create_2d_array {1 2 3 4 5 6 7 8 9 10 11 12} 3 4] eq {{1 2 3 4} {5 6 7 8} {9 10 11 12}}} {
puts "Test case 6 Passed"
} else {
error "Test case 6 Failed"
}

}

# Call the check procedure
check create_2d_array