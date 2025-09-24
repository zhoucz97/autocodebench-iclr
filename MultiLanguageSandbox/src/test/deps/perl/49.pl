
sub ascii_code {
    my ($char) = @_;
    return ord($char);
}


sub check {
    my $func = shift;
    die "Test failed!" unless &$func('A') == 65;
    die "Test failed!" unless &$func('B') == 66;
    die "Test failed!" unless &$func('C') == 67;
    die "Test failed!" unless &$func('a') == 97;
    die "Test failed!" unless &$func('b') == 98;
    die "Test failed!" unless &$func('c') == 99;
    die "Test failed!" unless &$func('0') == 48;
    die "Test failed!" unless &$func('1') == 49;
    die "Test failed!" unless &$func('2') == 50;
}

check(\&ascii_code);