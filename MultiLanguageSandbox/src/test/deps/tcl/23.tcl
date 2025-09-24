proc findTriplets {A B C} {
    set result {}
    # Generate all permutations of digits 1-9
    set digits {1 2 3 4 5 6 7 8 9}
    set perms [permute $digits]
    
    foreach perm $perms {
        # Split into three 3-digit numbers
        set num1 [lindex $perm 0][lindex $perm 1][lindex $perm 2]
        set num2 [lindex $perm 3][lindex $perm 4][lindex $perm 5]
        set num3 [lindex $perm 6][lindex $perm 7][lindex $perm 8]
        
        # Convert strings to integers
        set n1 [expr {$num1}]
        set n2 [expr {$num2}]
        set n3 [expr {$num3}]
        
        # Check the ratio conditions: A*n2 == B*n1 and A*n3 == C*n1
        if {($A * $n2 == $B * $n1) && ($A * $n3 == $C * $n1)} {
            lappend result [list $n1 $n2 $n3]
        }
    }
    
    if {[llength $result] == 0} {
        return "No!!!"
    } else {
        return $result
    }
}

# Helper procedure to generate all permutations of a list
proc permute {lst} {
    if {[llength $lst] <= 1} {
        return [list $lst]
    }
    set result {}
    for {set i 0} {$i < [llength $lst]} {incr i} {
        set first [lindex $lst $i]
        set rest [lreplace $lst $i $i]
        set subperms [permute $rest]
        foreach subperm $subperms {
            lappend result [concat [list $first] $subperm]
        }
    }
    return $result
}


proc check {findTriplets} {
# Test Cases
set test1 [findTriplets 1 2 3]
if {[llength $test1] > 0} {
puts "Test case 1 Passed"
} else {
error "Test case 1 Failed"
}

set test2 [findTriplets 2 3 4]
if {[llength $test2] > 0} {
puts "Test case 2 Passed"
} else {
error "Test case 2 Failed"
}

set test3 [findTriplets 1 1 1]
if {$test3 eq "No!!!"} {
puts "Test case 3 Passed"
} else {
error "Test case 3 Failed"
}

set test4 [findTriplets 3 3 3]
if {$test4 eq "No!!!"} {
puts "Test case 4 Passed"
} else {
error "Test case 4 Failed"
}

set test5 [findTriplets 5 7 9]
if {[llength $test5] > 0} {
puts "Test case 5 Passed"
} else {
error "Test case 5 Failed"
}
}

# Call the check procedure
check findTriplets