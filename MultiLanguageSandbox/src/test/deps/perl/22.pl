
sub find_min_n {
    my ($k) = @_;
    my $sum = 0;
    my $n = 0;
    
    while ($sum <= $k) {
        $n++;
        $sum += 1 / $n;
    }
    
    return $n;
}

# Test cases
print find_min_n(2), "\n";  # Output: 4
print find_min_n(3), "\n";  # Output: 11


sub check {
my $func = shift;
die "Test failed!" unless &$func(2) == 4;
die "Test failed!" unless &$func(3) == 11;
die "Test failed!" unless &$func(0) == 1;
die "Test failed!" unless &$func(1) == 2;
die "Test failed!" unless &$func(4) == 31;
die "Test failed!" unless &$func(5) == 83;
print "All tests passed!";
}

check(\&find_min_n);