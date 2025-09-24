
sub sorted_squares {
    my @nums = @_;
    my @squared = map { $_ * $_ } @nums;
    my @sorted = sort { $a <=> $b } @squared;
    return @sorted;
}


sub check {
my $func = shift;
die "Test failed!" unless join(',', @{$func->([-4, -1, 0, 3, 10])}) eq '0,1,9,16,100';
die "Test failed!" unless join(',', @{$func->([-7, -3, 2, 3, 11])}) eq '4,9,9,49,121';
die "Test failed!" unless join(',', @{$func->([1, 2, 3, 4])}) eq '1,4,9,16';
die "Test failed!" unless join(',', @{$func->([-1, 0, 1])}) eq '0,1,1';
print "All tests passed!\n";
}

check(\&sorted_squares);