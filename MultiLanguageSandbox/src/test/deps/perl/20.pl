
sub extract_numbers {
    my ($str) = @_;
    my @numbers;
    
    # Use a regular expression to find all sequences of digits
    while ($str =~ /(\d+)/g) {
        push @numbers, $1;
    }
    
    return \@numbers;
}


sub check {
my $func = shift;
die "Test failed!" unless join(',', @{$func->("The room numbers are 12, 34, and 56.")}) eq '12,34,56';
die "Test failed!" unless join(',', @{$func->("No numbers here!")}) eq '';
die "Test failed!" unless join(',', @{$func->("Version 2.0.1")}) eq '2,0,1';
die "Test failed!" unless join(',', @{$func->("100 ways to code 101")}) eq '100,101';
print "All tests passed!\n";
}

check(\&extract_numbers);