proc solveEquation {equation} {
    # Remove all spaces from the equation for easier processing
    set equation [string map {" " ""} $equation]
    
    # Split the equation into left and right parts based on '='
    set parts [split $equation "="]
    set left [lindex $parts 0]
    set right [lindex $parts 1]
    
    # Check if the unknown is on the right side
    if {[string match "*?" $right]} {
        # Solve for the right side (Z)
        set left_parts [split $left "+"]
        if {[llength $left_parts] == 1} {
            # It's a subtraction equation
            set left_parts [split $left "-"]
            set x [lindex $left_parts 0]
            set y [lindex $left_parts 1]
            return [expr {$x - $y}]
        } else {
            # It's an addition equation
            set x [lindex $left_parts 0]
            set y [lindex $left_parts 1]
            return [expr {$x + $y}]
        }
    } else {
        # The unknown is on the left side
        set op [string index $left 1]
        set left_num [string index $left 0]
        set right_num $right
        
        if {$op eq "+"} {
            # Equation is X+Y=Z, solve for X or Y
            if {[string match "?*" $left]} {
                # Solving for X: X = Z - Y
                set y [string range $left 2 end]
                return [expr {$right_num - $y}]
            } else {
                # Solving for Y: Y = Z - X
                set x [string index $left 0]
                return [expr {$right_num - $x}]
            }
        } else {
            # Equation is X-Y=Z, solve for X or Y
            if {[string match "?*" $left]} {
                # Solving for X: X = Z + Y
                set y [string range $left 2 end]
                return [expr {$right_num + $y}]
            } else {
                # Solving for Y: Y = X - Z
                set x [string index $left 0]
                return [expr {$x - $right_num}]
            }
        }
    }
}


proc testSolveEquation {} {
# Test case utility
proc assert_equal {actual expected message} {
if {$actual != $expected} {
error "$message. Expected $expected but got $actual"
} else {
puts "$message - Passed"
}
}

# Test cases
assert_equal [solveEquation "1+?=2"] 1 "Test case 1"
assert_equal [solveEquation "4-3=?"] 1 "Test case 2"
assert_equal [solveEquation "?-3=2"] 5 "Test case 3"
assert_equal [solveEquation "5+?=10"] 5 "Test case 4"
assert_equal [solveEquation "?+4=7"] 3 "Test case 5"
assert_equal [solveEquation "10-?=5"] 5 "Test case 6"
}

# Call the test procedure
testSolveEquation