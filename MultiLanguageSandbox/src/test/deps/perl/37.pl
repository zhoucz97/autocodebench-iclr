
use strict;
use warnings;

sub min_strokes_to_draw_graph {
    my ($n, $edges) = @_;
    
    # Initialize adjacency list and degree counts
    my @adj;
    my @degree;
    for my $i (1..$n) {
        $adj[$i] = [];
        $degree[$i] = 0;
    }
    
    # Build adjacency list and degree counts
    foreach my $edge (@$edges) {
        my ($u, $v) = @$edge;
        push @{$adj[$u]}, $v;
        push @{$adj[$v]}, $u;
        $degree[$u]++;
        $degree[$v]++;
    }
    
    # Find connected components using BFS
    my %visited;
    my @components;
    
    for my $i (1..$n) {
        next if exists $visited{$i};
        my @queue = ($i);
        $visited{$i} = 1;
        my @component;
        
        while (@queue) {
            my $current = shift @queue;
            push @component, $current;
            
            foreach my $neighbor (@{$adj[$current]}) {
                unless (exists $visited{$neighbor}) {
                    $visited{$neighbor} = 1;
                    push @queue, $neighbor;
                }
            }
        }
        push @components, \@component;
    }
    
    my $total_strokes = 0;
    
    foreach my $component (@components) {
        my $odd_count = 0;
        foreach my $vertex (@$component) {
            if ($degree[$vertex] % 2 != 0) {
                $odd_count++;
            }
        }
        # Strokes for this component is ceil(odd_count / 2)
        my $strokes = int(($odd_count + 1) / 2);
        $total_strokes += $strokes;
    }
    
    return $total_strokes;
}

# Test cases
print min_strokes_to_draw_graph(4, [[1, 2], [2, 3], [3, 4], [4, 1]]), "\n";  # Expected: 1
print min_strokes_to_draw_graph(5, [[1, 2], [2, 3], [3, 4], [4, 5], [5, 1]]), "\n";  # Expected: 1


sub check {
my $func = shift;
die "Test failed!" unless &$func(4, [[1, 2], [2, 3], [3, 4], [4, 1]]) == 1;
die "Test failed!" unless &$func(5, [[1, 2], [2, 3], [3, 4], [4, 5], [5, 1]]) == 1;
die "Test failed!" unless &$func(3, [[1, 2], [2, 3]]) == 1;
die "Test failed!" unless &$func(6, [[1, 2], [2, 3], [3, 4], [4, 5], [5, 6], [6, 1], [1, 3], [2, 4]]) == 2;
die "Test failed!" unless &$func(4, [[1, 2], [2, 3], [3, 4]]) == 1;
print "All tests passed!\n";
}

check(\&min_strokes_to_draw_graph);