proc max_min_gcd {nums} {
    # Find the maximum and minimum numbers in the list
    set max_num [lindex $nums 0]
    set min_num [lindex $nums 0]
    
    foreach num $nums {
        if {$num > $max_num} {
            set max_num $num
        }
        if {$num < $min_num} {
            set min_num $num
        }
    }
    
    # Function to compute the GCD of two numbers
    proc gcd {a b} {
        while {$b != 0} {
            set temp $b
            set b [expr {$a % $b}]
            set a $temp
        }
        return $a
    }
    
    # Compute the GCD of the maximum and minimum numbers
    return [gcd $max_num $min_num]
}

# Test cases

proc check {max_min_gcd} {
if {[max_min_gcd {2 4 6 8 10}] == 2} {
puts "Test case 1 Passed"
} else {
error "Test case 1 Failed"
}

if {[max_min_gcd {7 5 12 16 35}] == 5} {
puts "Test case 2 Passed"
} else {
error "Test case 2 Failed"
}

if {[max_min_gcd {15 45 75}] == 15} {
puts "Test case 3 Passed"
} else {
error "Test case 3 Failed"
}

if {[max_min_gcd {11 13 17 19}] == 1} {
puts "Test case 4 Passed"
} else {
error "Test case 4 Failed"
}

if {[max_min_gcd {20 40 60 80}] == 20} {
puts "Test case 5 Passed"
} else {
error "Test case 5 Failed"
}

if {[max_min_gcd {5 5 5 5}] == 5} {
puts "Test case 6 Passed"
} else {
error "Test case 6 Failed"
}

if {[max_min_gcd {100 200 300 400 500}] == 100} {
puts "Test case 7 Passed"
} else {
error "Test case 7 Failed"
}
}

# Call the check procedure
check max_min_gcd