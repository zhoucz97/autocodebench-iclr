proc min_moves_to_seat {seats students} {
    # Sort both seats and students lists
    set sorted_seats [lsort -integer $seats]
    set sorted_students [lsort -integer $students]
    
    set total_moves 0
    set length [llength $sorted_seats]
    
    for {set i 0} {$i < $length} {incr i} {
        set seat [lindex $sorted_seats $i]
        set student [lindex $sorted_students $i]
        set total_moves [expr {$total_moves + abs($seat - $student)}]
    }
    
    return $total_moves
}


proc check {min_moves_to_seat} {
if {[min_moves_to_seat {3 1 5} {2 7 4}] == 4} {
puts "Test case 1 Passed"
} else {
error "Test case 1 Failed"
}

if {[min_moves_to_seat {4 1 5 9} {1 3 2 6}] == 7} {
puts "Test case 2 Passed"
} else {
error "Test case 2 Failed"
}

if {[min_moves_to_seat {10 2 6} {3 8 9}] == 4} {
puts "Test case 3 Passed"
} else {
error "Test case 3 Failed"
}

if {[min_moves_to_seat {1 4 2 3} {4 1 3 2}] == 0} {
puts "Test case 4 Passed"
} else {
error "Test case 4 Failed"
}

if {[min_moves_to_seat {5 5 5} {1 2 3}] == 9} {
puts "Test case 5 Passed"
} else {
error "Test case 5 Failed"
}

if {[min_moves_to_seat {1 2 3} {3 2 1}] == 0} {
puts "Test case 6 Passed"
} else {
error "Test case 6 Failed"
}

if {[min_moves_to_seat {5 6 7} {5 6 7}] == 0} {
puts "Test case 7 Passed"
} else {
error "Test case 7 Failed"
}
}

# Call the check procedure
check min_moves_to_seat