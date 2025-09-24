proc count_quadruplets {nums} {
    set count 0
    set n [llength $nums]
    
    for {set a 0} {$a < $n - 3} {incr a} {
        for {set b [expr {$a + 1}]} {$b < $n - 2} {incr b} {
            for {set c [expr {$b + 1}]} {$c < $n - 1} {incr c} {
                set sum [expr {[lindex $nums $a] + [lindex $nums $b] + [lindex $nums $c]}]
                for {set d [expr {$c + 1}]} {$d < $n} {incr d} {
                    if {[lindex $nums $d] == $sum} {
                        incr count
                    }
                }
            }
        }
    }
    
    return $count
}


proc check {count_quadruplets} {
if {[count_quadruplets {1 2 3 6}] == 1} {
puts "Test case 1 Passed"
} else {
error "Test case 1 Failed"
}

if {[count_quadruplets {3 3 6 4 5}] == 0} {
puts "Test case 2 Passed"
} else {
error "Test case 2 Failed"
}

if {[count_quadruplets {1 1 1 1 1}] == 0} {
puts "Test case 3 Passed"
} else {
error "Test case 3 Failed"
}

if {[count_quadruplets {0 0 0 0}] == 1} {
puts "Test case 4 Passed"
} else {
error "Test case 4 Failed"
}

if {[count_quadruplets {1 2 3 4 5}] == 0} {
puts "Test case 5 Passed"
} else {
error "Test case 5 Failed"
}

if {[count_quadruplets {1 1 2 3 4 5 6}] == 5} {
puts "Test case 6 Passed"
} else {
error "Test case 6 Failed"
}

if {[count_quadruplets {-1 0 1 0}] == 1} {
puts "Test case 7 Passed"
} else {
error "Test case 7 Failed"
}
}

# Call the check procedure
check count_quadruplets