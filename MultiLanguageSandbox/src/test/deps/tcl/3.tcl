proc is_covered {ranges left right} {
    # Create a list to mark covered positions
    set covered [list]
    for {set i $left} {$i <= $right} {incr i} {
        lappend covered 0
    }
    
    # Mark all positions covered by the ranges
    foreach range $ranges {
        lassign $range start end
        for {set i $start} {$i <= $end} {incr i} {
            if {$i >= $left && $i <= $right} {
                set idx [expr {$i - $left}]
                lset covered $idx 1
            }
        }
    }
    
    # Check if all positions are covered
    foreach val $covered {
        if {$val == 0} {
            return false
        }
    }
    return true
}


proc check {is_covered} {
if {[is_covered {{1 2} {3 4} {5 6}} 2 5] == 1} {
puts "Test case 1 Passed"
} else {
error "Test case 1 Failed"
}

if {[is_covered {{1 10} {10 20}} 10 15] == 1} {
puts "Test case 2 Passed"
} else {
error "Test case 2 Failed"
}

if {[is_covered {{1 5} {10 15}} 5 10] == 0} {
puts "Test case 3 Passed"
} else {
error "Test case 3 Failed"
}

if {[is_covered {{1 3} {4 6} {7 9}} 1 9] == 1} {
puts "Test case 4 Passed"
} else {
error "Test case 4 Failed"
}

if {[is_covered {{5 10}} 1 4] == 0} {
puts "Test case 5 Passed"
} else {
error "Test case 5 Failed"
}

if {[is_covered {{0 0} {2 2} {4 4}} 0 4] == 0} {
puts "Test case 6 Passed"
} else {
error "Test case 6 Failed"
}

if {[is_covered {{10 20} {20 30} {30 40}} 15 25] == 1} {
puts "Test case 7 Passed"
} else {
error "Test case 7 Failed"
}
}

# Call the check procedure
check is_covered