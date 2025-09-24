
sub determine_sign {
    my ($N) = @_;
    if ($N > 0) {
        return 'positive';
    } elsif ($N == 0) {
        return 'zero';
    } else {
        return 'negative';
    }
}


sub check {
    my $func = shift;
    die "Test failed! Expected 'positive', got " . &$func(10) unless &$func(10) eq 'positive';
    die "Test failed! Expected 'zero', got " . &$func(0) unless &$func(0) eq 'zero';
    die "Test failed! Expected 'negative', got " . &$func(-1) unless &$func(-1) eq 'negative';
    die "Test failed! Expected 'negative', got " . &$func(-10000) unless &$func(-10000) eq 'negative';
    die "Test failed! Expected 'positive', got " . &$func(999) unless &$func(999) eq 'positive';
    print "All tests passed!\n";
}

check(\&determine_sign);