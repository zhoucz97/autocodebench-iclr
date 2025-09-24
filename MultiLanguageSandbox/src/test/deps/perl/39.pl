
use strict;
use warnings;

sub max_min_distance {
    my ($n, $m, $positions) = @_;
    
    # Sort the positions of the barns
    my @sorted_positions = sort { $a <=> $b } @$positions;
    
    # Binary search for the maximum minimum distance
    my $left = 0;
    my $right = $sorted_positions[-1] - $sorted_positions[0];
    my $result = 0;
    
    while ($left <= $right) {
        my $mid = int(($left + $right) / 2);
        
        # Check if it's possible to place all cows with at least 'mid' distance apart
        if (can_place_cows(\@sorted_positions, $m, $mid)) {
            $result = $mid;
            $left = $mid + 1;
        } else {
            $right = $mid - 1;
        }
    }
    
    return $result;
}

sub can_place_cows {
    my ($positions, $m, $min_distance) = @_;
    
    my $count = 1;
    my $last_position = $positions->[0];
    
    for (my $i = 1; $i < @$positions; $i++) {
        if ($positions->[$i] - $last_position >= $min_distance) {
            $count++;
            $last_position = $positions->[$i];
            
            # If we have placed all cows, return true
            if ($count >= $m) {
                return 1;
            }
        }
    }
    
    # If we couldn't place all cows, return false
    return 0;
}

# Example usage:
my $n = 4;
my $m = 2;
my $positions = [1, 3, 4, 10];

print max_min_distance($n, $m, $positions);  # Output: 9


sub test {
die "Test failed!" unless max_min_distance(4, 2, [1, 3, 4, 10]) == 9;
die "Test failed!" unless max_min_distance(5, 3, [1, 2, 8, 4, 9]) == 3;
die "Test failed!" unless max_min_distance(6, 2, [1, 4, 9, 12, 15, 17]) == 16;
die "Test failed!" unless max_min_distance(3, 2, [1, 5, 10]) == 9;
die "Test failed!" unless max_min_distance(7, 4, [1, 2, 4, 6, 7, 9, 11]) == 3;
print "All tests passed!\n";
}

test();