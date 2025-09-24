
sub projection_area {
    my ($grid) = @_;
    my $xy = 0;
    my $yz = 0;
    my $zx = 0;
    
    # Calculate xy projection: count of cells with value > 0
    foreach my $row (@$grid) {
        foreach my $cell (@$row) {
            $xy++ if $cell > 0;
        }
    }
    
    # Calculate yz projection: sum of max in each column
    my $cols = scalar @{$grid->[0]};
    for (my $j = 0; $j < $cols; $j++) {
        my $max_col = 0;
        foreach my $row (@$grid) {
            $max_col = $row->[$j] if $row->[$j] > $max_col;
        }
        $yz += $max_col;
    }
    
    # Calculate zx projection: sum of max in each row
    foreach my $row (@$grid) {
        my $max_row = 0;
        foreach my $cell (@$row) {
            $max_row = $cell if $cell > $max_row;
        }
        $zx += $max_row;
    }
    
    return $xy + $yz + $zx;
}


sub check {
my $func = shift;
die "Test failed!" unless &$func([[1,2],[3,4]]) == 17;
die "Test failed!" unless &$func([[2]]) == 5;
die "Test failed!" unless &$func([[1,0],[0,2]]) == 8;
die "Test failed!" unless &$func([[1,1,1],[1,0,1],[1,1,1]]) == 14;
print "All tests passed!\n";
}

check(\&projection_area);