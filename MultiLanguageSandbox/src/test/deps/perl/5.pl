
sub total_surface_area {
    my ($grid) = @_;
    my $rows = scalar @$grid;
    return 0 unless $rows > 0;
    my $cols = scalar @{$grid->[0]};
    my $total = 0;
    
    for my $i (0..$rows-1) {
        for my $j (0..$cols-1) {
            my $v = $grid->[$i][$j];
            next if $v == 0;
            # Add top and bottom surfaces (6 faces per cube, but top and bottom are only counted once per stack)
            $total += 2;  # top and bottom of the stack
            # Add the four sides
            # Check left neighbor
            if ($j > 0) {
                my $left = $grid->[$i][$j-1];
                $total += 2 * ($v < $left ? $v : $left);
            } else {
                $total += 2 * $v;
            }
            # Check right neighbor
            if ($j < $cols - 1) {
                my $right = $grid->[$i][$j+1];
                $total += 2 * ($v < $right ? $v : $right);
            } else {
                $total += 2 * $v;
            }
            # Check top neighbor
            if ($i > 0) {
                my $top = $grid->[$i-1][$j];
                $total += 2 * ($v < $top ? $v : $top);
            } else {
                $total += 2 * $v;
            }
            # Check bottom neighbor
            if ($i < $rows - 1) {
                my $bottom = $grid->[$i+1][$j];
                $total += 2 * ($v < $bottom ? $v : $bottom);
            } else {
                $total += 2 * $v;
            }
        }
    }
    return $total;
}
```

Wait, but the initial approach might not be correct. Let me re-examine the problem.

Actually, the correct approach is to consider that each cube contributes 6 units to the surface area, but adjacent cubes (in any of the six directions) reduce the total by 2 units per adjacent pair (one from each cube). 

So for each cube in the grid, the total surface area is 6 * v, but for each adjacent cube (in any of the four directions in the grid, since the grid is 2D), the overlapping surfaces are subtracted. 

But the grid represents stacks of cubes in 2D, so adjacent stacks in the grid are considered to be adjacent in 3D space (left, right, top, bottom directions). 

So for each cell (i,j) with value v, the total surface area contributed is:

- Top and bottom: 2 (top and bottom of the stack)
- Four sides: for each of the four directions (left, right, top, bottom), if there's an adjacent cell, the overlapping area is 2 * min(v, adjacent_v). Otherwise, it's 2 * v.

So the total surface area is:

For each cell (i,j):

- Add 2 (top and bottom)
- For each of the four neighbors (left, right, top, bottom):
   - If the neighbor exists, add 2 * min(current_v, neighbor_v)
   - Else, add 2 * current_v

But wait, no. Actually, each cube in the stack contributes 6 surfaces, but adjacent cubes hide surfaces. So for each cube in the stack, the surfaces hidden are:

- For each of the six faces (top, bottom, left, right, front, back), if there's an adjacent cube in that direction, then that face is not exposed.

But since the grid is 2D, the "front" and "back" directions don't exist (assuming the grid is flat). So only left, right, top, bottom, and the vertical (up and down within the stack) directions matter.

But the grid represents stacks of cubes in 2D, so each cell (i,j) has v cubes stacked vertically. The surfaces are:

- For each cube in the stack (from 1 to v):
   - The top face of the top cube is exposed (1 unit)
   - The bottom face of the bottom cube is exposed (1 unit)
   - The four side faces: for each side, if there's an adjacent stack in that direction, the number of hidden faces is the minimum height between the current stack and the adjacent stack. Otherwise, all side faces are exposed.

So the total surface area is:

For each cell (i,j) with value v:

- Add 2 (top and bottom of the stack)
- For each of the four directions (left, right, top, bottom):
   - If the neighbor exists, add 2 * min(v, neighbor_v)
   - Else, add 2 * v

But this seems to match the initial approach. Let's test with the examples.

Example 1: [[2]]

- Only one cell (0,0) with v=2.
- Top and bottom: 2
- No neighbors, so each of the four sides contributes 2*2 = 4 each. Total sides: 4*4 = 16.
- But 2 (top-bottom) + 16 (sides) = 18, which doesn't match the expected 10. So this approach is incorrect.

Hmm, this suggests that the initial approach is wrong. Let me think differently.

Each cube in the stack contributes 6 surfaces, but adjacent cubes hide surfaces. So for a single stack of v cubes:

- The top cube has 5 exposed surfaces (top is exposed, bottom is hidden by the cube below)
- The middle cubes each have 4 exposed surfaces (top and bottom hidden)
- The bottom cube has 5 exposed surfaces (bottom is exposed, top is hidden by the cube above)

So total surfaces for a stack of v cubes is:

- Top cube: 5
- Middle cubes (v-2): 4 each
- Bottom cube: 5

Total = 5 + 4*(v-2) + 5 = 10 + 4*(v-2) = 4v + 2.

For v=2: 4*2 + 2 = 10. Which matches the first example.

Now, for adjacent stacks, the side surfaces that face each other are hidden. So for each pair of adjacent stacks, the number of hidden surfaces is 2 * min(height1, height2).

So the total surface area is:

For each stack, add (4*v + 2)

Then, for each pair of adjacent stacks, subtract 2 * min(v1, v2).

So for the first example [[2]]:

- One stack: 4*2 + 2 = 10. No adjacent stacks. Total 10. Correct.

Second example [[1,2],[3,4]]:

Stacks:

(0,0):1, (0,1):2

(1,0):3, (1,1):4

Total without adjacents:

(4*1 + 2) + (4*2 + 2) + (4*3 + 2) + (4*4 + 2) = 6 + 10 + 14 + 18 = 48.

Now subtract the adjacent pairs:

- (0,0) and (0,1): min(1,2)=1. Subtract 2*1=2.
- (0,0) and (1,0): min(1,3)=1. Subtract 2*1=2.
- (0,1) and (1,1): min(2,4)=2. Subtract 2*2=4.
- (1,0) and (1,1): min(3,4)=3. Subtract 2*3=6.

Total subtraction: 2 + 2 + 4 + 6 = 14.

48 - 14 = 34. Which matches the second example.

So the correct approach is:

1. For each cell (i,j) with value v, add (4*v + 2) to the total.
2. For each adjacent cell (in four directions), subtract 2 * min(current_v, neighbor_v) from the total.

Now, let's write the Perl function based on this logic.

```perl
sub total_surface_area {
    my ($grid) = @_;
    my $rows = scalar @$grid;
    return 0 unless $rows > 0;
    my $cols = scalar @{$grid->[0]};
    my $total = 0;
    
    for my $i (0..$rows-1) {
        for my $j (0..$cols-1) {
            my $v = $grid->[$i][$j];
            $total += 4 * $v + 2;
        }
    }
    
    for my $i (0..$rows-1) {
        for my $j (0..$cols-1) {
            my $v = $grid->[$i][$j];
            # Check right neighbor
            if ($j < $cols - 1) {
                my $right = $grid->[$i][$j+1];
                $total -= 2 * ($v < $right ? $v : $right);
            }
            # Check bottom neighbor
            if ($i < $rows - 1) {
                my $bottom = $grid->[$i+1][$j];
                $total -= 2 * ($v < $bottom ? $v : $bottom);
            }
        }
    }
    return $total;
}
```

This should work. Let's test the examples.

First example: [[2]]

- Only one cell (0,0) with v=2.
- Total starts at 4*2 + 2 = 10.
- No neighbors, so no subtraction. Total remains 10. Correct.

Second example: [[1,2],[3,4]]

- Cells:
  - (0,0):1 → 4*1 + 2 = 6
  - (0,1):2 → 4*2 + 2 = 10
  - (1,0):3 → 4*3 + 2 = 14
  - (1,1):4 → 4*4 + 2 = 18
- Total before subtraction: 6 + 10 + 14 + 18 = 48.
- Subtractions:
  - (0,0) right neighbor (0,1): min(1,2)=1 → subtract 2*1=2.
  - (0,0) bottom neighbor (1,0): min(1,3)=1 → subtract 2*1=2.
  - (0,1) right neighbor: none (j=1 is last column)
  - (0,1) bottom neighbor (1,1): min(2,4)=2 → subtract 2*2=4.
  - (1,0) right neighbor (1,1): min(3,4)=3 → subtract 2*3=6.
  - (1,0) bottom neighbor: none (i=1 is last row)
  - (1,1) right and bottom: none.
- Total subtraction: 2 + 2 + 4 + 6 = 14.
- Final total: 48 - 14 = 34. Correct.

So the function works.

Final Perl function:

```perl
sub total_surface_area {
    my ($grid) = @_;
    my $rows = scalar @$grid;
    return 0 unless $rows > 0;
    my $cols = scalar @{$grid->[0]};
    my $total = 0;
    
    for my $i (0..$rows-1) {
        for my $j (0..$cols-1) {
            my $v = $grid->[$i][$j];
            $total += 4 * $v + 2;
        }
    }
    
    for my $i (0..$rows-1) {
        for my $j (0..$cols-1) {
            my $v = $grid->[$i][$j];
            # Check right neighbor
            if ($j < $cols - 1) {
                my $right = $grid->[$i][$j+1];
                $total -= 2 * ($v < $right ? $v : $right);
            }
            # Check bottom neighbor
            if ($i < $rows - 1) {
                my $bottom = $grid->[$i+1][$j];
                $total -= 2 * ($v < $bottom ? $v : $bottom);
            }
        }
    }
    return $total;
}


sub check {
my $func = shift;
die "Test failed!" unless &$func([[2]]) == 10;
die "Test failed!" unless &$func([[1,2],[3,4]]) == 34;
die "Test failed!" unless &$func([[1,0],[0,2]]) == 16;
die "Test failed!" unless &$func([[1,1,1],[1,0,1],[1,1,1]]) == 32;
print "All tests passed!\n";
}

check(\&total_surface_area);