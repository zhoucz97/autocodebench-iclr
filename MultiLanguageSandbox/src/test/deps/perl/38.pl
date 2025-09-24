
use strict;
use warnings;

sub min_circle_crosses {
    my ($circles_x, $circles_y, $radii, $point1, $point2) = @_;
    
    my $x1 = $point1->[0];
    my $y1 = $point1->[1];
    my $x2 = $point2->[0];
    my $y2 = $point2->[1];
    
    my $count = 0;
    
    for my $i (0 .. $#{$circles_x}) {
        my $cx = $circles_x->[$i];
        my $cy = $circles_y->[$i];
        my $r = $radii->[$i];
        
        # Vector from point1 to point2
        my $dx = $x2 - $x1;
        my $dy = $y2 - $y1;
        
        # Vector from point1 to circle center
        my $fx = $cx - $x1;
        my $fy = $cy - $y1;
        
        # Dot product of (point2 - point1) and (circle center - point1)
        my $dot = $dx * $fx + $dy * $fy;
        
        # Squared length of the line segment
        my $len_sq = $dx * $dx + $dy * $dy;
        
        # Parameter t of the closest point on the line to the circle center
        my $t = $dot / $len_sq;
        
        # Clamp t to [0, 1] to get the closest point on the line segment
        $t = 0 if $t < 0;
        $t = 1 if $t > 1;
        
        # Closest point on the line segment to the circle center
        my $closest_x = $x1 + $t * $dx;
        my $closest_y = $y1 + $t * $dy;
        
        # Distance squared from closest point to circle center
        my $dist_sq = ($closest_x - $cx)**2 + ($closest_y - $cy)**2;
        
        if ($dist_sq <= $r**2) {
            # Check if the closest point is not one of the endpoints (since the line doesn't start or end on any circle)
            # But according to problem statement, the line doesn't start or end on any circle, so t should not be 0 or 1 unless the circle is at an endpoint, which is not the case here.
            # So, if dist_sq <= r^2, then the line segment intersects the circle.
            $count++;
        }
    }
    
    return $count;
}

# Example usage:
my $result1 = min_circle_crosses(
    [1,-3,2,5,-4,12,12],
    [1,-1,2,5,5,1,1],
    [8,1,2,1,1,1,2],
    [-5, 1],
    [12, 1]
);
print "$result1\n";  # Expected output: 3

my $result2 = min_circle_crosses(
    [-4, 4],
    [4, -4],
    [2, 2],
    [-5, 0],
    [5, 0]
);
print "$result2\n";  # Expected output: 0


sub check {
my $func = shift;
die "Test failed!" unless &$func([1, -3, 2], [1, -1, 2], [1, 1, 1], [-2, 1], [3, 2]) == 0;
die "Test failed!" unless &$func([-4, 4], [4, -4], [2, 2], [-5, 0], [5, 0]) == 0;
die "Test failed!" unless &$func([0, 5], [0, 5], [3, 3], [-7, 0], [7, 0]) == 0;
die "Test failed!" if &$func([0, 5], [0, 5], [3, 3], [-7, 0], [7, 0]) != 0;
die "Test failed!" unless &$func([-2, 2], [-2, 2], [1, 1], [-3, -3], [3, 3]) == 0;
die "Test failed!" unless &$func([1,-3,2,5,-4,12,12], [1,-1,2,5,5,1,1], [8,1,2,1,1,1,2], [-5, 1], [12, 1]) == 3;
print "All tests passed!\n";
}

check(\&min_circle_crosses);