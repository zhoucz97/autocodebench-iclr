proc count_leap_years {x y} {
    set leap_years {}
    set count 0

    for {set year $x} {$year <= $y} {incr year} {
        if {($year % 4 == 0 && $year % 100 != 0) || ($year % 400 == 0)} {
            lappend leap_years $year
            incr count
        }
    }

    return [list $count $leap_years]
}


proc test_count_leap_years {} {
# Test case utility
proc assert_equal {actual expected message} {
if {$actual ne $expected} {
error "$message. Expected $expected but got $actual"
} else {
puts "$message - Passed"
}
}

# Test cases
assert_equal [count_leap_years 2000 2020] "6\n2000 2004 2008 2012 2016 2020" "Test case 1"
assert_equal [count_leap_years 1900 1920] "5\n1904 1908 1912 1916 1920" "Test case 2"
assert_equal [count_leap_years 1950 1960] "3\n1952 1956 1960" "Test case 3"
assert_equal [count_leap_years 1985 1995] "2\n1988 1992" "Test case 4" 
assert_equal [count_leap_years 2010 2015] "1\n2012" "Test case 5"
}

# Call the test procedure
test_count_leap_years