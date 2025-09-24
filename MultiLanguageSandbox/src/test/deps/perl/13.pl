
sub find_repeated_element {
    my ($nums) = @_;
    
    # Create a hash to count occurrences of each element
    my %count;
    
    # Count occurrences of each element
    foreach my $num (@$nums) {
        $count{$num}++;
    }
    
    # Find the element that appears more than once (since all others are unique)
    foreach my $key (keys %count) {
        if ($count{$key} > 1) {
            return $key;
        }
    }
    
    # This line should theoretically never be reached given the problem constraints
    return undef;
}


sub check {
my $func = shift;
die "Test failed!" unless &$func([1,2,3,3]) == 3;
die "Test failed!" unless &$func([2,1,2,5,3,2]) == 2;
die "Test failed!" unless &$func([5,1,5,2,5,3,5,4]) == 5;
die "Test failed!" unless &$func([10,9,10]) == 10;
print "All tests passed!\n";
}

check(\&find_repeated_element);