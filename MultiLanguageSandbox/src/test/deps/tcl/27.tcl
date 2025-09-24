proc update_student_info {students} {
    set updated_students {}
    foreach student $students {
        lassign $student name age score
        # Increase age by 1
        incr age
        # Increase NOIP score by 20%, capped at 600
        set new_score [expr {int($score * 1.2)}]
        if {$new_score > 600} {
            set new_score 600
        }
        # Append the updated student information to the list
        lappend updated_students [list $name $age $new_score]
    }
    return $updated_students
}


proc check {update_student_info} {
# Test case utility
proc assert_equal {actual expected message} {
set actual_str ""
foreach student $actual {
append actual_str [join $student " "] " "
}
set actual_str [string trim $actual_str]

if {$actual_str != $expected} {
error "$message. Expected $expected but got $actual_str"
} else {
puts "$message - Passed"
}
}

# Test cases
assert_equal [update_student_info {{"Alice" 15 300} {"Bob" 17 450} {"Charlie" 16 550}}] "Alice 16 360 Bob 18 540 Charlie 17 600" "Test case 1"
assert_equal [update_student_info {{"Daniel" 14 100} {"Eve" 18 580} {"Frank" 17 500}}] "Daniel 15 120 Eve 19 600 Frank 18 600" "Test case 2"
assert_equal [update_student_info {{"Grace" 15 0} {"Henry" 16 600}}] "Grace 16 0 Henry 17 600" "Test case 3"
}

# Call the check procedure
check update_student_info