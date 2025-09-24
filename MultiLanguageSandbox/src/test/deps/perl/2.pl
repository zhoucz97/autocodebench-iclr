
sub transpose_matrix {
    my ($matrix) = @_;
    
    # Determine the number of rows and columns in the original matrix
    my $rows = scalar @$matrix;
    return [] unless $rows > 0;  # Handle empty matrix case
    
    my $cols = scalar @{$matrix->[0]};
    
    # Create the transposed matrix
    my @transposed;
    for my $col (0..$cols-1) {
        my @new_row;
        for my $row (0..$rows-1) {
            push @new_row, $matrix->[$row][$col];
        }
        push @transposed, \@new_row;
    }
    
    return \@transposed;
}


sub check {
my $func = shift;
use Test::More tests => 4;
is_deeply &$func([[1, 2, 3], [4, 5, 6]]), [[1, 4], [2, 5], [3, 6]], "Test 1 passed!";
is_deeply &$func([[1, 2], [3, 4], [5, 6]]), [[1, 3, 5], [2, 4, 6]], "Test 2 passed!";
is_deeply &$func([[1]]), [[1]], "Test 3 passed!";
is_deeply &$func([[1, 2], [3, 4]]), [[1, 3], [2, 4]], "Test 4 passed!";
}

check(\&transpose_matrix);