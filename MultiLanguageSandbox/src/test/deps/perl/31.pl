
use strict;
use warnings;

sub count_maze_paths {
    my ($N, $M, $T, $SX, $SY, $FX, $FY, @obstacles) = @_;
    
    # Create a 2D array to represent the maze (1-based indexing)
    my @maze;
    for my $i (1..$N) {
        for my $j (1..$M) {
            $maze[$i][$j] = 0;  # 0 means passable
        }
    }
    
    # Mark obstacles as 1 (impassable)
    foreach my $obs (@obstacles) {
        my ($x, $y) = @$obs;
        $maze[$x][$y] = 1;
    }
    
    # Check if start or end is blocked
    return 0 if $maze[$SX][$SY] == 1 || $maze[$FX][$FY] == 1;
    
    # Directions: up, down, left, right
    my @dirs = ( [-1, 0], [1, 0], [0, -1], [0, 1] );
    
    my $count = 0;
    
    # DFS subroutine
    my $dfs;
    $dfs = sub {
        my ($x, $y, $visited) = @_;
        
        # If we reached the finish
        if ($x == $FX && $y == $FY) {
            $count++;
            return;
        }
        
        # Mark current cell as visited
        $visited->{$x}{$y} = 1;
        
        # Explore all four directions
        foreach my $dir (@dirs) {
            my ($dx, $dy) = @$dir;
            my $nx = $x + $dx;
            my $ny = $y + $dy;
            
            # Check if new position is valid and not visited
            if ($nx >= 1 && $nx <= $N && $ny >= 1 && $ny <= $M &&
                $maze[$nx][$ny] == 0 && !exists $visited->{$nx}{$ny}) {
                
                $dfs->($nx, $ny, $visited);
            }
        }
        
        # Backtrack: unmark current cell
        delete $visited->{$x}{$y};
    };
    
    # Start DFS from the starting position
    $dfs->($SX, $SY, {});
    
    return $count;
}

# Test cases
print count_maze_paths(3, 3, 2, 1, 1, 3, 3, [[1, 2], [2, 1]]), "\n";  # Expected: 0
print count_maze_paths(2, 2, 1, 1, 1, 2, 2, [[2, 1]]), "\n";          # Expected: 1


sub check {
die "Test failed!" unless count_maze_paths(3, 3, 2, 1, 1, 3, 3, [[1, 2], [2, 1]]) == 0;
die "Test failed!" unless count_maze_paths(2, 2, 1, 1, 1, 2, 2, [[2, 1]]) == 1;
die "Test failed!" unless count_maze_paths(4, 4, 3, 1, 1, 4, 4, [[2, 2], [2, 3], [3, 2]]) == 4;
die "Test failed!" unless count_maze_paths(3, 3, 0, 1, 1, 3, 3, []) == 12;
die "Test failed!" if count_maze_paths(2, 2, 2, 1, 1, 2, 2, [[1, 2], [2, 1]]) != 0;
print "All tests passed!\n";
}

check();