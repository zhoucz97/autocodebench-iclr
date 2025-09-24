
sub calculate_mod_power {
    my ($a, $b, $p) = @_;
    my $result = 1;
    
    # Handle the case where p is 1 (any number mod 1 is 0)
    if ($p == 1) {
        return "$a^$b mod $p=0";
    }
    
    # Compute a^b mod p using modular exponentiation
    $a = $a % $p;  # Ensure a is within the range of p
    while ($b > 0) {
        # If b is odd, multiply the result with a
        if ($b % 2 == 1) {
            $result = ($result * $a) % $p;
        }
        # Square a and reduce modulo p
        $a = ($a * $a) % $p;
        # Divide b by 2
        $b = int($b / 2);
    }
    
    return "$a^$b mod $p=$result";
}


sub check {
my $func = shift;
die "Test failed!" unless &$func(3, 4, 5) eq "3^4 mod 5=1";
die "Test failed!" unless &$func(7, 2, 10) eq "7^2 mod 10=9";
die "Test failed!" unless &$func(5, 3, 6) eq "5^3 mod 6=5";
die "Test failed!" unless &$func(10, 2, 9) eq "10^2 mod 9=1";
print "All tests passed!\n";
}

check(\&calculate_mod_power);