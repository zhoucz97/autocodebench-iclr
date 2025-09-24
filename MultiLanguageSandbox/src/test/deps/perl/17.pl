
sub numbers_above_average {
    my @array = @_;
    return [] unless @array;  # Return empty array if input is empty
    
    my $sum = 0;
    $sum += $_ for @array;
    my $average = $sum / scalar @array;
    
    my @result = grep { $_ > $average } @array;
    return \@result;
}


sub check {
my $func = shift;
die "Test failed!" unless join(',', @{$func->([1, 2, 3, 4, 5])}) eq '4,5';
die "Test failed!" unless join(',', @{$func->([10, 20, 30, 40, 50])}) eq '40,50';
die "Test failed!" unless join(',', @{$func->([1, 1, 1, 1, 10])}) eq '10';
die "Test failed!" unless join(',', @{$func->([15, 15, 15, 15, 15])}) eq '';
print "All tests passed!\n";
}

check(\&numbers_above_average);