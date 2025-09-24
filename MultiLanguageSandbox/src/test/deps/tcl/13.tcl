proc construct_common_elements_array {nums1 nums2 nums3} {
    # Create frequency dictionary to count occurrences of each element
    set freq [dict create]
    
    # Helper procedure to update frequency counts
    proc update_freq {freq_list elements} {
        foreach elem $elements {
            dict incr freq_list $elem
        }
        return $freq_list
    }
    
    # Update frequency for all three arrays
    set freq [update_freq $freq $nums1]
    set freq [update_freq $freq $nums2]
    set freq [update_freq $freq $nums3]
    
    # Collect elements that appear at least twice
    set result {}
    dict for {elem count} $freq {
        if {$count >= 2} {
            lappend result $elem
        }
    }
    
    # Sort the result in ascending order
    set result [lsort -integer $result]
    
    return $result
}


proc check {} {
if {[construct_common_elements_array {1 2 3} {4 3 2} {5 6 2}] eq {2 3}} {
puts "Test case 1 Passed"
} else {
error "Test case 1 Failed"
}

if {[construct_common_elements_array {3 1} {2 3} {1}] eq {1 3}} {
puts "Test case 2 Passed"
} else {
error "Test case 2 Failed"
}

if {[construct_common_elements_array {1 4 5} {2 4 6} {3 4 7}] eq {4}} {
puts "Test case 3 Passed"
} else {
error "Test case 3 Failed"
}

if {[construct_common_elements_array {10 20 30} {30 40 50} {50 60 70}] eq {30 50}} {
puts "Test case 4 Passed"
} else {
error "Test case 4 Failed"
}

if {[construct_common_elements_array {1 2 3} {4 5 6} {7 8 9}] eq {}} {
puts "Test case 5 Passed"
} else {
error "Test case 5 Failed"
}

if {[construct_common_elements_array {1 1 1} {1 1} {1}] eq {1}} {
puts "Test case 6 Passed"
} else {
error "Test case 6 Failed"
}

if {[construct_common_elements_array {1 2 3 4} {2 3 4 5} {3 4 5 6}] eq {2 3 4 5}} {
puts "Test case 7 Passed"
} else {
error "Test case 7 Failed"
}
}

# Call the check procedure
check