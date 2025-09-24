<?php


/**
 * Counts the number of unique paths an ant can take to move from the bottom-left to the top-right corner of a grid.
 * The grid is defined by m rows and n columns. The ant starts at (1,1) and can only move right or up due to an injury.
 * - For a 1x1 grid (m = 1, n = 1), there is only 1 path.
 * - For a 2x3 grid (m = 2, n = 3), there are 3 possible paths.
 * Input: Two integers, m and n, representing the number of rows and columns of the grid.
 * Output: Returns the count of unique paths from (1,1) to (m,n).
 */

 function countAntPaths($m, $n): int {

    // The number of paths is C(m+n-2, m-1) or C(m+n-2, n-1)
    // We can compute this using the multiplicative formula to avoid large intermediate values
    $total = $m + $n - 2;
    $k = min($m - 1, $n - 1);
    $result = 1;
    
    for ($i = 1; $i <= $k; $i++) {
        $result = $result * ($total - $k + $i) / $i;
    }
    
    return (int)$result;
}
function main(){
    assert(countAntPaths(1, 1) === 1); // One path in a 1x1 grid
    assert(countAntPaths(2, 3) === 3); // Three paths in a 2x3 grid
    assert(countAntPaths(3, 3) === 6); // Six paths in a 3x3 grid    
    assert(countAntPaths(7, 13) === 18564);    
}


main();
?>