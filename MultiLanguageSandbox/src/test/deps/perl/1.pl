
sub has_close_elements {
    my ($numbers, $threshold) = @_;
    
    # Sort the numbers to easily check adjacent elements
    my @sorted = sort { $a <=> $b } @$numbers;
    
    # Check each pair of adjacent elements
    for (my $i = 0; $i < @sorted - 1; $i++) {
        if (abs($sorted[$i] - $sorted[$i+1]) < $threshold) {
            return 1;  # True in Perl (returns 1 for true)
        }
    }
    
    return 0;  # False in Perl (returns 0 for false)
}


# Test cases
sub check {
my $func = shift;
die "Test failed!" unless &$func([1.0, 2.0, 3.9, 4.0, 5.0, 2.2], 0.3);
die "Test failed!" if &$func([1.0, 2.0, 3.9, 4.0, 5.0, 2.2], 0.05);
die "Test failed!" unless &$func([1.0, 2.0, 5.9, 4.0, 5.0], 0.95);
die "Test failed!" if &$func([1.0, 2.0, 5.9, 4.0, 5.0], 0.8);
die "Test failed!" unless &$func([1.0, 2.0, 3.0, 4.0, 5.0, 2.0], 0.1);
die "Test failed!" unless &$func([1.1, 2.2, 3.1, 4.1, 5.1], 1.0);
die "Test failed!" if &$func([1.1, 2.2, 3.1, 4.1, 5.1], 0.5);
print "All tests passed!\n";
}

check(\&has_close_elements);