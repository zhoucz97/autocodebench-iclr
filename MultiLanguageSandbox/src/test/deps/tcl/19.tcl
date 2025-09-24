proc find_even_numbers {digits} {
    set result {}
    set len [llength $digits]
    
    # Generate all permutations of three digits
    for {set i 0} {$i < $len} {incr i} {
        for {set j 0} {$j < $len} {incr j} {
            if {$i == $j} continue
            for {set k 0} {$k < $len} {incr k} {
                if {$j == $k || $i == $k} continue
                
                # Get the three digits
                set d1 [lindex $digits $i]
                set d2 [lindex $digits $j]
                set d3 [lindex $digits $k]
                
                # Form the number and check if it's even and doesn't have leading zero
                set num [expr {$d1 * 100 + $d2 * 10 + $d3}]
                if {$d1 != 0 && $num % 2 == 0} {
                    lappend result $num
                }
            }
        }
    }
    
    # Remove duplicates and sort the result
    set unique_result [lsort -unique $result]
    return $unique_result
}


proc check {find_even_numbers} {
if {[find_even_numbers {1 2 3}] eq {132 312}} {
puts "Test case 1 Passed"
} else {
error "Test case 1 Failed"
}

if {[find_even_numbers {0 1 2}] eq {102 120 210}} {
puts "Test case 2 Passed"
} else {
error "Test case 2 Failed"
}

if {[find_even_numbers {4 6 8}] eq {468 486 648 684 846 864}} {
puts "Test case 3 Passed"
} else {
error "Test case 3 Failed"
}

if {[find_even_numbers {0 0 2}] eq {200}} {
puts "Test case 4 Passed"
} else {
error "Test case 4 Failed"
}

if {[find_even_numbers {5 7 9}] eq {}} {
puts "Test case 5 Passed"
} else {
error "Test case 5 Failed"
}

if {[find_even_numbers {1 1 2}] eq {112}} {
puts "Test case 6 Passed"
} else {
error "Test case 6 Failed"
}

if {[find_even_numbers {0 1 1}] eq {110}} {
puts "Test case 7 Passed"
} else {
error "Test case 7 Failed"
}
}

# Call the check procedure
check find_even_numbers