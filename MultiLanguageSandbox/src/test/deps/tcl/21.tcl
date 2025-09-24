proc find_max_population_year {logs} {
    array set population {}
    
    foreach log $logs {
        lassign $log birth death
        for {set year $birth} {$year < $death} {incr year} {
            if {[info exists population($year)]} {
                incr population($year)
            } else {
                set population($year) 1
            }
        }
    }
    
    set max_pop -1
    set result_year 0
    
    foreach year [lsort -integer [array names population]] {
        if {$population($year) > $max_pop} {
            set max_pop $population($year)
            set result_year $year
        }
    }
    
    return $result_year
}


proc check {find_max_population_year} {
if {[find_max_population_year {{1900 1950} {1940 2000} {1950 2000}}] == 1940} {
puts "Test case 1 Passed"
} else {
error "Test case 1 Failed"
}

if {[find_max_population_year {{2000 2010} {1975 2005} {1975 2003}}] == 2000} {
puts "Test case 2 Passed"
} else {
error "Test case 2 Failed"
}

if {[find_max_population_year {{1990 1995} {1995 2000} {2000 2005}}] == 1990} {
puts "Test case 3 Passed"
} else {
error "Test case 3 Failed"
}

if {[find_max_population_year {{1800 1801} {1800 1802} {1801 1802}}] == 1800} {
puts "Test case 4 Passed"
} else {
error "Test case 4 Failed"
}

if {[find_max_population_year {{1950 1960} {1950 1960} {1950 1960}}] == 1950} {
puts "Test case 5 Passed"
} else {
error "Test case 5 Failed"
}

if {[find_max_population_year {{1600 1700} {1601 1701} {1602 1702}}] == 1602} {
puts "Test case 6 Passed"
} else {
error "Test case 6 Failed"
}

if {[find_max_population_year {{2000 2005} {2001 2006} {2002 2007}}] == 2002} {
puts "Test case 7 Passed"
} else {
error "Test case 7 Failed"
}
}

# Call the check procedure
check find_max_population_year