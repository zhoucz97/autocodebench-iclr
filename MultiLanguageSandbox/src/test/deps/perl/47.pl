
sub convert_int_to_bool_to_int {
    my ($input) = @_;
    # Convert integer to boolean (non-zero becomes true/1, zero becomes false/0)
    my $bool = $input ? 1 : 0;
    # Convert boolean back to integer (true remains 1, false becomes 0)
    return $bool;
}


sub check {
    my $func = shift;
    die "Test failed! Expected 1, got " . &$func(3) unless &$func(3) == 1;
    die "Test failed! Expected 0, got " . &$func(0) unless &$func(0) == 0;
    die "Test failed! Expected 1, got " . &$func(-1) unless &$func(-1) == 1;
    die "Test failed! Expected 1, got " . &$func(100) unless &$func(100) == 1;
    die "Test failed! Expected 1, got " . &$func(-100) unless &$func(-100) == 1;
}

check(\&convert_int_to_bool_to_int);