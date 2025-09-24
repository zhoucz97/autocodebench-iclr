
sub ascii_to_char {
    my ($ascii_code) = @_;
    return chr($ascii_code);
}


sub check {
    my $func = shift;
    die "Test failed! Expected 'A', got '" . &$func(65) . "'" unless &$func(65) eq 'A';
    die "Test failed! Expected 'a', got '" . &$func(97) . "'" unless &$func(97) eq 'a';
    die "Test failed! Expected '0', got '" . &$func(48) . "'" unless &$func(48) eq '0';
    die "Test failed! Expected '@', got '" . &$func(64) . "'" unless &$func(64) eq '@';
    die "Test failed! Expected '}', got '" . &$func(125) . "'" unless &$func(125) eq '}';
}

check(\&ascii_to_char);