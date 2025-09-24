proc max_distance_different_colors {colors} {
    set max_dist 0
    set n [llength $colors]
    for {set i 0} {$i < $n} {incr i} {
        for {set j [expr {$i + 1}]} {$j < $n} {incr j} {
            if {[lindex $colors $i] != [lindex $colors $j]} {
                set current_dist [expr {$j - $i}]
                if {$current_dist > $max_dist} {
                    set max_dist $current_dist
                }
            }
        }
    }
    return $max_dist
}


proc check {max_distance_different_colors} {
if {[max_distance_different_colors {1 1 1 6 1 1 1}] == 3} {
puts "Test case 1 Passed"
} else {
error "Test case 1 Failed"
}

if {[max_distance_different_colors {1 2 1 2 1 2 1}] == 5} {
puts "Test case 2 Passed"
} else {
error "Test case 2 Failed"
}

if {[max_distance_different_colors {1 1 1 1 1}] == 0} {
puts "Test case 3 Passed"
} else {
error "Test case 3 Failed"
}

if {[max_distance_different_colors {2 2 3 3 3 2 2}] == 4} {
puts "Test case 4 Passed"
} else {
error "Test case 4 Failed"
}

if {[max_distance_different_colors {1 3 2 4 5}] == 4} {
puts "Test case 5 Passed"
} else {
error "Test case 5 Failed"
}

if {[max_distance_different_colors {1}] == 0} {
puts "Test case 6 Passed"
} else {
error "Test case 6 Failed"
}

if {[max_distance_different_colors {1 2 3 4 5 6}] == 5} {
puts "Test case 7 Passed"
} else {
error "Test case 7 Failed"
}
}

# Call the check procedure
check max_distance_different_colors