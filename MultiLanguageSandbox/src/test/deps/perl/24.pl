
sub remaining_trees {
    my ($length, $regions) = @_;
    
    # Initialize a hash to mark removed positions
    my %removed;
    
    # Mark all positions in the regions as removed
    foreach my $region (@$regions) {
        my ($start, $end) = @$region;
        for my $pos ($start..$end) {
            $removed{$pos} = 1;
        }
    }
    
    # Count the number of positions not marked as removed
    my $count = 0;
    for my $pos (1..$length) {
        $count++ unless exists $removed{$pos};
    }
    
    return $count;
}
```

However, this approach has O(n*m) complexity where n is the length of the road and m is the number of regions, which might be inefficient for very large roads. Here's a more efficient version that uses interval merging:

```perl
sub remaining_trees {
    my ($length, $regions) = @_;
    
    # Sort regions by start position
    my @sorted_regions = sort { $a->[0] <=> $b->[0] } @$regions;
    
    # Merge overlapping or adjacent regions
    my @merged;
    foreach my $region (@sorted_regions) {
        if (@merged && $region->[0] <= $merged[-1][1] + 1) {
            # Merge with last region
            $merged[-1][1] = $region->[1] if $region->[1] > $merged[-1][1];
        } else {
            push @merged, [@$region];
        }
    }
    
    # Calculate total removed trees
    my $removed = 0;
    foreach my $region (@merged) {
        $removed += $region->[1] - $region->[0] + 1;
    }
    
    # Total trees is length (assuming trees at 1..length), so remaining is length - removed
    return $length - $removed;
}


sub check {
my $func = shift;
die "Test failed!" unless &$func(1000, [[200, 300], [400, 500], [600, 700]]) == 698;
die "Test failed!" unless &$func(500, [[100, 200], [300, 400]]) == 299;
die "Test failed!" unless &$func(100, [[20, 30], [40, 50], [60, 70]]) == 68;
die "Test failed!" unless &$func(50, [[10, 20], [30, 40]]) == 29;
die "Test failed!" unless &$func(1000, [[200, 300], [400, 500], [600, 700], [800, 900]]) == 597;
print "All tests passed";
}

check(\&remaining_trees);