proc remove_duplicates_and_sort {numbers} {
    # Remove duplicates by converting the list to a dictionary (keys are unique)
    set unique_numbers [dict keys [dict create {*}[lmap num $numbers {list $num 1}]]]
    
    # Sort the unique numbers in ascending order
    set sorted_numbers [lsort -integer $unique_numbers]
    
    # Get the count of unique numbers
    set count [llength $sorted_numbers]
    
    # Return the count followed by the sorted unique numbers
    return [concat $count $sorted_numbers]
}


proc test_remove_duplicates_and_sort {} {
# Test case utility
proc assert_equal {actual expected message} {
if {$actual ne $expected} {
error "$message. Expected $expected but got $actual"
} else {
puts "$message - Passed"
}
}

# Test cases
assert_equal [remove_duplicates_and_sort {20 40 32 67 40 20 89 300 400 15}] "8 15 20 32 40 67 89 300 400" "Test case 1"
assert_equal [remove_duplicates_and_sort {5 1 2 2 4 5}] "4 1 2 4 5" "Test case 2"
assert_equal [remove_duplicates_and_sort {3 3 3 3}] "1 3" "Test case 3"
assert_equal [remove_duplicates_and_sort {10 9 8 7 6 5 4 3 2 1}] "10 1 2 3 4 5 6 7 8 9 10" "Test case 4"
assert_equal [remove_duplicates_and_sort {100 200 300 200 100}] "3 100 200 300" "Test case 5"
}

# Call the test procedure
test_remove_duplicates_and_sort