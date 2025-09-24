
sub max_herb_value {
    my ($t, $herbs) = @_;
    
    # Initialize a DP array where dp[i] is the max value for time i
    my @dp;
    $dp[0] = 0;  # Base case: 0 time gives 0 value
    
    for my $i (1..$t) {
        $dp[$i] = 0;
        foreach my $herb (@$herbs) {
            my ($time, $value) = @$herb;
            if ($time <= $i) {
                if ($dp[$i - $time] + $value > $dp[$i]) {
                    $dp[$i] = $dp[$i - $time] + $value;
                }
            }
        }
    }
    
    return $dp[$t];
}


sub test_max_herb_value {
die "Test failed!" unless max_herb_value(10, [[1, 5], [2, 10], [3, 15]]) == 50;
die "Test failed!" unless max_herb_value(5, [[2, 4], [3, 5]]) == 9;
die "Test failed!" unless max_herb_value(7, [[1, 2], [2, 5], [3, 7]]) == 17;
die "Test failed!" if max_herb_value(0, [[1, 10], [2, 20]]) != 0;
die "Test failed!" unless max_herb_value(20, [[5, 10], [4, 7], [3, 5]]) == 40;
print "All tests passed!\n";
}

test_max_herb_value();