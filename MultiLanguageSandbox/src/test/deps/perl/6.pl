
sub is_monotonic {
    my ($nums) = @_;
    
    return 1 if @$nums <= 1;  # empty or single-element arrays are considered monotonic
    
    my $increasing = 1;
    my $decreasing = 1;
    
    for my $i (1..$#$nums) {
        if ($nums->[$i] > $nums->[$i-1]) {
            $decreasing = 0;
        } elsif ($nums->[$i] < $nums->[$i-1]) {
            $increasing = 0;
        }
        
        # Early exit if neither condition is possible
        last unless $increasing || $decreasing;
    }
    
    return $increasing || $decreasing;
}


sub check {
my $func = shift;
die "Test failed!" unless &$func([1, 2, 2, 3]);
die "Test failed!" unless &$func([6, 5, 4, 4]);
die "Test failed!" if &$func([1, 3, 2]);
die "Test failed!" unless &$func([10]);
die "Test failed!" unless &$func([5, 5, 5, 5]);
print "All tests passed!\n";
}

check(\&is_monotonic);