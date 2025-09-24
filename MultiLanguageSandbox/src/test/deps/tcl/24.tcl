proc calculate_triangle_perimeter {x1 y1 x2 y2 x3 y3} {
    # Calculate the distance between two points (x1, y1) and (x2, y2)
    proc distance {x1 y1 x2 y2} {
        set dx [expr {$x2 - $x1}]
        set dy [expr {$y2 - $y1}]
        return [expr {sqrt($dx * $dx + $dy * $dy)}]
    }

    # Calculate the lengths of the three sides of the triangle
    set side1 [distance $x1 $y1 $x2 $y2]
    set side2 [distance $x2 $y2 $x3 $y3]
    set side3 [distance $x3 $y3 $x1 $y1]

    # Calculate the perimeter by summing the lengths of the three sides
    set perimeter [expr {$side1 + $side2 + $side3}]

    # Round the perimeter to two decimal places
    return [format "%.2f" $perimeter]
}


proc check {calculate_triangle_perimeter} {
# Test case utility
proc assert_equal {actual expected message} {
if {$actual != $expected} {
error "$message. Expected $expected but got $actual"
} else {
puts "$message - Passed"
}
}

# Test cases
assert_equal [calculate_triangle_perimeter 0 0 3 0 0 4] "12.00" "Test case 1"
assert_equal [calculate_triangle_perimeter 1 1 4 1 1 5] "12.00" "Test case 2"
assert_equal [calculate_triangle_perimeter 0 0 0 6 8 0] "24.00" "Test case 3"
assert_equal [calculate_triangle_perimeter 1 2 3 4 5 6] "11.31" "Test case 4"
assert_equal [calculate_triangle_perimeter -1 -2 -3 -4 -5 -6] "11.31" "Test case 5"
assert_equal [calculate_triangle_perimeter 10 10 20 20 30 30] "56.57" "Test case 6"
}

# Call the check procedure
check calculate_triangle_perimeter