
sub max_triangle_perimeter {
    my ($nums) = @_;
    my @sorted = sort { $b <=> $a } @$nums;  # Sort in descending order
    for (my $i = 0; $i < @sorted - 2; $i++) {
        my $a = $sorted[$i];
        my $b = $sorted[$i+1];
        my $c = $sorted[$i+2];
        if ($b + $c > $a) {
            return $a + $b + $c;
        }
    }
    return 0;
}


sub check {
my $func = shift;
die "Test failed!" unless &$func([2, 1, 2]) == 5;
die "Test failed!" if &$func([1, 2, 1]) != 0;
die "Test failed!" unless &$func([3, 2, 3, 4]) == 10;
die "Test failed!" unless &$func([3, 6, 2, 3]) == 8;
print "All tests passed!\n";
}

check(\&max_triangle_perimeter);