
sub generate_snail_matrix {
    my ($n) = @_;
    
    # Initialize an n x n matrix filled with zeros
    my @matrix;
    for my $i (0..$n-1) {
        $matrix[$i] = [(0) x $n];
    }
    
    my $num = 1;
    my $top = 0;
    my $bottom = $n - 1;
    my $left = 0;
    my $right = $n - 1;
    
    while ($num <= $n * $n) {
        # Traverse from left to right on top row
        for my $i ($left..$right) {
            $matrix[$top][$i] = $num++;
        }
        $top++;
        
        # Traverse from top to bottom on right column
        for my $i ($top..$bottom) {
            $matrix[$i][$right] = $num++;
        }
        $right--;
        
        # Traverse from right to left on bottom row
        for my $i (reverse $left..$right) {
            $matrix[$bottom][$i] = $num++;
        }
        $bottom--;
        
        # Traverse from bottom to top on left column
        for my $i (reverse $top..$bottom) {
            $matrix[$i][$left] = $num++;
        }
        $left++;
    }
    
    return \@matrix;
}


sub test_snail_matrix {
my $func = shift;
use Data::Dumper;

my $result1 = &$func(3);
die "Test failed for n=3!" unless Dumper($result1) eq Dumper([[1,2,3],[8,9,4],[7,6,5]]);

my $result2 = &$func(2);
die "Test failed for n=2!" unless Dumper($result2) eq Dumper([[1,2],[4,3]]);

my $result3 = &$func(5);
die "Test failed for n=5!" unless Dumper($result3) eq Dumper([[1,2,3,4,5],[16,17,18,19,6],[15,24,25,20,7],[14,23,22,21,8],[13,12,11,10,9]]);

print "All tests passed!\n";
}

test_snail_matrix(\&generate_snail_matrix);