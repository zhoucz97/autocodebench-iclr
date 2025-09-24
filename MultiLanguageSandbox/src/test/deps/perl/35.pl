
use strict;
use warnings;

sub min_level_diff_sum {
    my ($levels, $k) = @_;
    my @sorted_levels = sort { $a <=> $b } @$levels;
    my %used;
    my $total_diff = 0;
    
    for (my $i = 0; $i < $k; $i++) {
        my $min_diff = 999999; # A large number to start with
        my $best_pair = [-1, -1];
        
        # Find the pair with the smallest difference not yet fully used
        for (my $j = 0; $j < @sorted_levels - 1; $j++) {
            if ($used{$j} < 2 && $used{$j+1} < 2) {
                my $diff = abs($sorted_levels[$j] - $sorted_levels[$j+1]);
                if ($diff < $min_diff) {
                    $min_diff = $diff;
                    $best_pair = [$j, $j+1];
                }
            }
        }
        
        if ($best_pair->[0] != -1) {
            $total_diff += $min_diff;
            $used{$best_pair->[0]}++;
            $used{$best_pair->[1]}++;
        } else {
            # No more possible pairs, though k should be feasible per problem constraints
            last;
        }
    }
    
    return $total_diff;
}

# Test cases
print min_level_diff_sum([10, 20, 15, 30, 25], 2), "\n"; # Expected output: 10
print min_level_diff_sum([5, 3, 8, 12, 7, 10], 3), "\n"; # Expected output: 5
```

Wait, but the first test case [10, 20, 15, 30, 25] sorted becomes [10, 15, 20, 25, 30]. The smallest differences are between 10-15 (5), 15-20 (5), 20-25 (5), 25-30 (5). For k=2, we can pick two pairs with difference 5 each, totaling 10. So the code should work.

The second test case [5, 3, 8, 12, 7, 10] sorted is [3, 5, 7, 8, 10, 12]. The smallest differences are 3-5 (2), 5-7 (2), 7-8 (1), 8-10 (2), 10-12 (2). For k=3, we can pick 7-8 (1), then 3-5 (2), and 5-7 (2) but 5 is already used once (in 3-5), so next is 8-10 (2). Total is 1 + 2 + 2 = 5. So the code should handle this.

But wait, the code may not handle the case where after picking a pair, the next smallest available pair is chosen. Let me think again.

In the second example, after sorting [3,5,7,8,10,12], the differences are:

- 3-5: 2
- 5-7: 2
- 7-8: 1
- 8-10: 2
- 10-12: 2

For k=3:

1. Pick the smallest available difference: 7-8 (1). Used counts: 7 and 8 are incremented to 1.
2. Next smallest available: 3-5 (2). Used counts: 3 and 5 are incremented to 1.
3. Next smallest available: 5-7. But 5 is already used once (in 3-5), and 7 is used once (in 7-8). So we can't use 5-7 again because each player can be used up to two times per color, but in this context, each game uses two distinct players, and each player can be in at most two games (one as white, one as black). So 5 has been used once (as part of 3-5), and 7 has been used once (as part of 7-8). So they can be used again in another game. So the next smallest is 5-7 (2). Total becomes 1 + 2 + 2 = 5.

So the code should work.

But let me think of another example to test the code.

Suppose levels are [1, 2, 3, 4] and k=2.

Sorted: [1,2,3,4].

Differences:

1-2: 1

2-3: 1

3-4: 1.

Pick two pairs: 1-2 and 2-3. But 2 is used twice (once in 1-2 and once in 2-3). Is this allowed? The problem states each player can play up to two games, one with black and one with white. So yes, 2 can be in two games, once as white and once as black. So total difference is 1 + 1 = 2.

The code would pick 1-2 (difference 1), then 2-3 (difference 1), totaling 2. Correct.

Another example: levels [1, 3, 4, 6], k=2.

Sorted: [1,3,4,6].

Differences:

1-3: 2

3-4: 1

4-6: 2.

Pick the smallest: 3-4 (1), then next smallest available: 1-3 (2). Total 3. But is there a better way? Yes, pick 1-3 (2) and 4-6 (2), total 4. Or 3-4 (1) and 1-3 (2), total 3. Or 3-4 (1) and 4-6 (2), total 3. The minimal total is 3. So the greedy approach works here.

So the code seems correct.

But wait, in the first test case, the sorted levels are [10,15,20,25,30]. The differences are 5,5,5,5. For k=2, pick two pairs, each with difference 5, total 10. Correct.

Second test case: sorted [3,5,7,8,10,12]. Differences 2,2,1,2,2. Pick 7-8 (1), then 3-5 (2), then 5-7 (2). Total 5. Correct.

So the code should pass the test cases.

But let me think of a case where the greedy approach might not work. Suppose levels [1, 10, 11, 20], k=2.

Sorted: [1,10,11,20].

Differences:

1-10: 9

10-11: 1

11-20: 9.

Pick the smallest: 10-11 (1). Then next smallest: 1-10 (9). Total 10. But is there a better way? Yes, pick 1-10 (9) and 11-20 (9), total 18. Or 10-11 (1) and 1-10 (9), total 10. Or 10-11 (1) and 11-20 (9), total 10. The minimal is 10. So greedy works.

Another case: levels [1,2,3,100], k=2.

Sorted: [1,2,3,100].

Differences:

1-2: 1

2-3: 1

3-100: 97.

Pick 1-2 (1), then 2-3 (1). Total 2. Correct.

So the greedy approach seems to work.

Thus, the Perl function should be:

```perl
use strict;
use warnings;

sub min_level_diff_sum {
    my ($levels, $k) = @_;
    my @sorted_levels = sort { $a <=> $b } @$levels;
    my %used;
    my $total_diff = 0;
    
    for (my $i = 0; $i < $k; $i++) {
        my $min_diff = 999999; # A large number to start with
        my $best_pair = [-1, -1];
        
        # Find the pair with the smallest difference not yet fully used
        for (my $j = 0; $j < @sorted_levels - 1; $j++) {
            if ($used{$j} < 2 && $used{$j+1} < 2) {
                my $diff = abs($sorted_levels[$j] - $sorted_levels[$j+1]);
                if ($diff < $min_diff) {
                    $min_diff = $diff;
                    $best_pair = [$j, $j+1];
                }
            }
        }
        
        if ($best_pair->[0] != -1) {
            $total_diff += $min_diff;
            $used{$best_pair->[0]}++;
            $used{$best_pair->[1]}++;
        } else {
            # No more possible pairs, though k should be feasible per problem constraints
            last;
        }
    }
    
    return $total_diff;
}

# Test cases
print min_level_diff_sum([10, 20, 15, 30, 25], 2), "\n"; # Expected output: 10
print min_level_diff_sum([5, 3, 8, 12, 7, 10], 3), "\n"; # Expected output: 5


sub test_min_level_diff_sum {
die "Test failed!" unless min_level_diff_sum([10, 20, 15, 30, 25], 2) == 10;
die "Test failed!" unless min_level_diff_sum([5, 3, 8, 12, 7, 10], 3) == 5;
die "Test failed!" unless min_level_diff_sum([40, 35, 30, 25, 20, 15], 2) == 10;
die "Test failed!" unless min_level_diff_sum([1, 2, 3, 4, 5, 6, 7], 3) == 3;
die "Test failed!" unless min_level_diff_sum([21, 19, 17, 15, 13, 11, 9], 4) == 8;
print "All tests passed!\n";
}

test_min_level_diff_sum();