proc count_prime_sums {n k numbers} {
    set count 0
    set combs [combinations $numbers $k]
    foreach comb $combs {
        set sum 0
        foreach num $comb {
            set sum [expr {$sum + $num}]
        }
        if {[is_prime $sum]} {
            incr count
        }
    }
    return $count
}


proc check {count_prime_sums} {
proc assert_equal {actual expected message} {
if {$actual != $expected} {
error "$message. Expected $expected but got $actual"
} else {
puts "$message - Passed"
}
}

assert_equal [count_prime_sums 5 3 {2 3 5 7 11}] 2 "Test case 1"
assert_equal [count_prime_sums 4 2 {4 6 8 9}] 2 "Test case 2"
assert_equal [count_prime_sums 6 4 {1 3 5 7 9 11}] 0 "Test case 3"
assert_equal [count_prime_sums 3 2 {2 5 6}] 2 "Test case 4"
assert_equal [count_prime_sums 4 3 {10 12 14 15}] 2 "Test case 5"
}

check count_prime_sums