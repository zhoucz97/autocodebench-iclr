proc final_value_after_operations {operations} {
    set x 0
    foreach op $operations {
        if {$op eq "X++" || $op eq "++X"} {
            incr x
        } elseif {$op eq "X--" || $op eq "--X"} {
            incr x -1
        }
    }
    return $x
}


proc check {final_value_after_operations} {
if {[final_value_after_operations {"X++" "++X" "X++" "--X"}] == 2} {
puts "Test case 1 Passed"
} else {
error "Test case 1 Failed"
}

if {[final_value_after_operations {"--X" "X--" "X++" "X++"}] == 0} {
puts "Test case 2 Passed"
} else {
error "Test case 2 Failed"
}

if {[final_value_after_operations {"++X" "++X" "++X"}] == 3} {
puts "Test case 3 Passed"
} else {
error "Test case 3 Failed"
}

if {[final_value_after_operations {"X--" "X--" "X--"}] == -3} {
puts "Test case 4 Passed"
} else {
error "Test case 4 Failed"
}

if {[final_value_after_operations {"X++" "X++" "X--" "X--"}] == 0} {
puts "Test case 5 Passed"
} else {
error "Test case 5 Failed"
}

if {[final_value_after_operations {"--X" "X++" "X++"}] == 1} {
puts "Test case 6 Passed"
} else {
error "Test case 6 Failed"
}

if {[final_value_after_operations {"++X" "++X" "X--"}] == 1} {
puts "Test case 7 Passed"
} else {
error "Test case 7 Failed"
}
}

# Call the check procedure
check final_value_after_operations