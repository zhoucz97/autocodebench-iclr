
sub calculate_expression {
    my ($a, $b, $c) = @_;
    return ($a + $b) * $c;
}


sub check {
    my $func = shift;
    die "Test failed!" unless &$func(2, 3, 5) == 25;
    die "Test failed!" unless &$func(10, 20, 3) == 90;
    die "Test failed!" unless &$func(-1, 1, 1) == 0;
    die "Test failed!" unless &$func(0, 0, 1) == 0;
}

check(\&calculate_expression);