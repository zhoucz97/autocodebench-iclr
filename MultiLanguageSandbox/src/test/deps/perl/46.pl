
sub reverse_three_digit_number {
    my ($number) = @_;
    # Convert the number to a string to handle each digit individually
    my $str = "$number";
    # Extract each digit
    my $hundreds = substr($str, 0, 1);
    my $tens = substr($str, 1, 1);
    my $units = substr($str, 2, 1);
    # Reverse the digits and return as a string
    return $units . $tens . $hundreds;
}


sub check {
    my $func = shift;
    die "Test failed! Expected '853', got '" . &$func(358) . "'" unless &$func(358) eq '853';
    die "Test failed! Expected '001', got '" . &$func(100) . "'" unless &$func(100) eq '001';
    die "Test failed! Expected '876', got '" . &$func(678) . "'" unless &$func(678) eq '876';
}

check(\&reverse_three_digit_number);