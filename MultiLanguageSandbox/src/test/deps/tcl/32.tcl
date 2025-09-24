proc calculate_minimal_dissatisfaction {schoolScores studentScores} {
    # Sort both lists to facilitate the two-pointer approach
    set sortedSchools [lsort -integer $schoolScores]
    set sortedStudents [lsort -integer $studentScores]
    
    set totalDissatisfaction 0
    set schoolPtr 0
    set studentPtr 0
    set n [llength $sortedSchools]
    set m [llength $sortedStudents]
    
    while {$studentPtr < $m && $schoolPtr < $n} {
        set currentStudent [lindex $sortedStudents $studentPtr]
        set currentSchool [lindex $sortedSchools $schoolPtr]
        
        if {$currentSchool <= $currentStudent} {
            # Check if the next school is closer to the current student
            if {$schoolPtr + 1 < $n} {
                set nextSchool [lindex $sortedSchools [expr {$schoolPtr + 1}]]
                if {abs($nextSchool - $currentStudent) < abs($currentSchool - $currentStudent)} {
                    incr schoolPtr
                    set currentSchool $nextSchool
                }
            }
            # Add the minimal difference
            set totalDissatisfaction [expr {$totalDissatisfaction + abs($currentSchool - $currentStudent)}]
            incr studentPtr
            incr schoolPtr
        } else {
            # Move the school pointer forward as the current school is higher than the student
            incr schoolPtr
        }
    }
    
    return $totalDissatisfaction
}


proc check {calculate_minimal_dissatisfaction} {
proc assert_equal {actual expected message} {
if {$actual != $expected} {
error "$message. Expected $expected but got $actual"
} else {
puts "$message - Passed"
}
}

assert_equal [calculate_minimal_dissatisfaction {510 600 580 700} {505 610 570}] 25 "Test case 1"
assert_equal [calculate_minimal_dissatisfaction {500 550 600 650} {520 560 590}] 40 "Test case 2"
assert_equal [calculate_minimal_dissatisfaction {520 580 640} {500 630 550}] 60 "Test case 3"
assert_equal [calculate_minimal_dissatisfaction {530 560 590 620} {550 570 540}] 30 "Test case 4"
assert_equal [calculate_minimal_dissatisfaction {500 530 560 590} {510 540 570}] 30 "Test case 5"
}

check calculate_minimal_dissatisfaction