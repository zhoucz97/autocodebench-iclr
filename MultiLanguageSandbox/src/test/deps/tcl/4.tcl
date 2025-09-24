proc can_be_increasing {nums} {
    set count 0
    for {set i 1} {$i < [llength $nums]} {incr i} {
        set prev [lindex $nums [expr {$i - 1}]]
        set current [lindex $nums $i]
        if {$prev >= $current} {
            incr count
            if {$count > 1} {
                return false
            }
            # Check if we can remove either the previous or current element
            if {$i < 2 || [lindex $nums [expr {$i - 2}]] < $current} {
                # Remove previous element (i-1)
                continue
            } elseif {$i == [llength $nums] - 1 || $prev < [lindex $nums [expr {$i + 1}]]} {
                # Remove current element (i)
                continue
            } else {
                return false
            }
        }
    }
    return true
}


proc check {can_be_increasing} {
if {[can_be_increasing {1 3 2 4}] == 1} {
puts "Test case 1 Passed"
} else {
error "Test case 1 Failed"
}

if {[can_be_increasing {2 3 1 2}] == 0} {
puts "Test case 2 Passed"
} else {
error "Test case 2 Failed"
}

if {[can_be_increasing {1 2 3}] == 1} {
puts "Test case 3 Passed"
} else {
error "Test case 3 Failed"
}

if {[can_be_increasing {10 1 2 3 4}] == 1} {
puts "Test case 4 Passed"
} else {
error "Test case 4 Failed"
}

if {[can_be_increasing {1 2 10 3 4}] == 1} {
puts "Test case 5 Passed"
} else {
error "Test case 5 Failed"
}

if {[can_be_increasing {1 2 3 4 3}] == 1} {
puts "Test case 6 Passed"
} else {
error "Test case 6 Failed"
}

if {[can_be_increasing {5 4 3 2 1}] == 0} {
puts "Test case 7 Passed"
} else {
error "Test case 7 Failed"
}
}

# Call the check procedure
check can_be_increasing