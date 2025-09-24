
sub is_mountain_array {
    my ($arr) = @_;
    
    # Check if array has at least 3 elements
    return 0 if scalar(@$arr) < 3;
    
    my $n = scalar(@$arr);
    my $i = 0;
    
    # Walk up the mountain
    while ($i < $n - 1 && $arr->[$i] < $arr->[$i + 1]) {
        $i++;
    }
    
    # Peak can't be first or last
    return 0 if ($i == 0 || $i == $n - 1);
    
    # Walk down the mountain
    while ($i < $n - 1 && $arr->[$i] > $arr->[$i + 1]) {
        $i++;
    }
    
    # If we reached the end, it's a mountain array
    return $i == $n - 1 ? 1 : 0;
}


sub check {
my $func = shift;
die "Test failed!" unless &$func([0, 3, 2, 1]);
die "Test failed!" if &$func([2, 1]);
die "Test failed!" if &$func([3, 5, 5]);
die "Test failed!" unless &$func([0, 2, 3, 4, 5, 2, 1, 0]);
die "Test failed!" if &$func([0, 2, 3, 3, 5, 2, 1, 0]);
print "All tests passed!\n";
}

check(\&is_mountain_array);