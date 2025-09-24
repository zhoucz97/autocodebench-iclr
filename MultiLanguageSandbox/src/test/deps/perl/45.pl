
sub calculate_arithmetic_sequence_nth_term {
    my ($a1, $a2, $n) = @_;
    
    # Calculate the common difference d
    my $d = $a2 - $a1;
    
    # Calculate the nth term using the formula: a_n = a1 + (n-1)*d
    my $nth_term = $a1 + ($n - 1) * $d;
    
    return $nth_term;
}

# Example usage:
# print calculate_arithmetic_sequence_nth_term(1, 4, 100);  # Output: 298


sub check {
    my $func = shift;
    die "Test failed! Expected 298, got " . &$func(1, 4, 100) unless &$func(1, 4, 100) == 298;
    die "Test failed! Expected 8, got " . &$func(2, 5, 3) unless &$func(2, 5, 3) == 8;
    die "Test failed! Expected 11, got " . &$func(-1, 3, 5) unless &$func(-1, 3, 5) == 15;
    die "Test failed! Expected 200, got " . &$func(1, 3, 100) unless &$func(1, 3, 100) == 199;
    die "Test failed! Expected -97, got " . &$func(-100, -99, 4) unless &$func(-100, -99, 4) == -97;
}

check(\&calculate_arithmetic_sequence_nth_term);