<?php


/**
 * Calculates the maximum number of distinct letters one can pass through in a letter matrix.
 * Starting from the top-left corner, you can move up, down, left, or right, but cannot visit a cell more than once.
 * This function finds the maximum number of distinct letters that can be traversed in the matrix.
 * 
 * Inputs:
 * - $rows (int): The number of rows in the matrix.
 * - $cols (int): The number of columns in the matrix.
 * - $matrix (array): A 2D array representing the letter matrix.
 *
 * Output:
 * - Returns an integer representing the maximum number of distinct letters that can be traversed.
 *
 * Example:
 * maxDistinctLetters(3, 3, [['A', 'B', 'C'], ['D', 'E', 'F'], ['G', 'H', 'I']]) should return 9.
 */

function maxDistinctLetters($rows, $cols, $matrix) {

    $visited = array_fill(0, $rows, array_fill(0, $cols, false));
    $maxDistinct = 0;
    
    // Directions: up, down, left, right
    $directions = [[-1, 0], [1, 0], [0, -1], [0, 1]];
    
    $dfs = function($i, $j, &$visited, &$distinctLetters) use (&$dfs, $directions, $rows, $cols, $matrix, &$maxDistinct) {
        $currentLetter = $matrix[$i][$j];
        if (!in_array($currentLetter, $distinctLetters)) {
            $distinctLetters[] = $currentLetter;
            if (count($distinctLetters) > $maxDistinct) {
                $maxDistinct = count($distinctLetters);
            }
        }
        
        foreach ($directions as $dir) {
            $ni = $i + $dir[0];
            $nj = $j + $dir[1];
            
            if ($ni >= 0 && $ni < $rows && $nj >= 0 && $nj < $cols && !$visited[$ni][$nj]) {
                $visited[$ni][$nj] = true;
                $dfs($ni, $nj, $visited, $distinctLetters);
                $visited[$ni][$nj] = false;
            }
        }
    };
    
    $visited[0][0] = true;
    $initialDistinct = [$matrix[0][0]];
    $dfs(0, 0, $visited, $initialDistinct);
    
    return $maxDistinct;
}
function main(){
    $matrix = [
        ['H', 'F', 'D', 'F', 'F', 'B'],
        ['A', 'J', 'H', 'G', 'D', 'H'],
        ['D', 'G', 'A', 'G', 'E', 'H']
    ];
    assert(maxDistinctLetters(3, 6, $matrix) === 6);
  
    $matrix = [
        ['H', 'F', 'Y' ],
        ['A', 'J', 'Z' ],
        ['D', 'G', 'A']
    ];
    assert(maxDistinctLetters(3, 3, $matrix) === 8);

    $matrix = [
        ['A', 'D', 'G' ],
        ['B', 'E', 'H' ],
        ['C', 'F', 'I']
    ];
    assert(maxDistinctLetters(3, 3, $matrix) === 9);

}


main();
?>