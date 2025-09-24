
sub min_deletion_size {
    my ($grid) = @_;
    return 0 unless @$grid;  # handle empty grid case
    
    my $rows = scalar @$grid;
    my $cols = length($grid->[0]);
    my $count = 0;
    
    for my $col (0..$cols-1) {
        for my $row (1..$rows-1) {
            if (substr($grid->[$row-1], $col, 1) gt substr($grid->[$row], $col, 1)) {
                $count++;
                last;
            }
        }
    }
    
    return $count;
}


sub check {
my $func = shift;
die "Test failed!" unless &$func(["abc", "bce", "cae"]) == 1;
die "Test failed!" unless &$func(["cba", "daf", "ghi"]) == 1;
die "Test failed!" unless &$func(["a", "b"]) == 0;
die "Test failed!" unless &$func(["zyx", "wvu", "tsr"]) == 3;
print "All tests passed!\n";
}

check(\&min_deletion_size);