
sub multiply_two_numbers {
    my ($A, $B) = @_;
    return $A * $B;
}


sub check {
    my $func = shift;
    die "Test failed! Expected 12, got " . &$func(3, 4) unless &$func(3, 4) == 12;
    die "Test failed! Expected 648, got " . &$func(36, 18) unless &$func(36, 18) == 648;
    die "Test failed! Expected 50000, got " . &$func(1, 50000) unless &$func(1, 50000) == 50000;
    die "Test failed! Expected 250000, got " . &$func(50, 5000) unless &$func(50, 5000) == 250000;
    die "Test failed! Expected 1000000, got " . &$func(200, 5000) unless &$func(200, 5000) == 1000000;

}

check(\&multiply_two_numbers);