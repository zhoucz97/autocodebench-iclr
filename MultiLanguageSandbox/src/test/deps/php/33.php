<?php

/**
    * Counts the number of flu infections in a dormitory grid after a certain number of days.
    * The dormitory is represented as an n*n grid where each cell can have:
    * - '.' representing a healthy person
    * - '#' representing an empty room
    * - '@' representing a person infected with the flu
    * Each day, infected individuals spread the flu to adjacent (up, down, left, right) healthy individuals.
    * The function calculates the total number of infected individuals after 'm' days.
    * Input: 
    *   - $dormitory: A 2D array representing the n*n dormitory grid.
    *   - $days: An integer representing the number of days.
    * Output: Returns the total number of flu infections after 'm' days.
    * Example:
    *   Input: [['.','.','.','.','#'], 
                ['.','#','.','@','.'], 
                ['.','#','@','.','.'], 
                ['#','.','.','.','.'], 
                ['#','.','.','.','.']], 4
    *   Output: 16
*/
function countFluInfections($dormitory, $days) {

    $n = count($dormitory);
    $infected = [];
    
    // Initialize the queue with initial infected positions
    for ($i = 0; $i < $n; $i++) {
        for ($j = 0; $j < $n; $j++) {
            if ($dormitory[$i][$j] == '@') {
                $infected[] = [$i, $j];
            }
        }
    }
    
    $directions = [[-1, 0], [1, 0], [0, -1], [0, 1]];
    
    for ($day = 0; $day < $days; $day++) {
        $newInfected = [];
        foreach ($infected as $cell) {
            list($x, $y) = $cell;
            foreach ($directions as $dir) {
                $nx = $x + $dir[0];
                $ny = $y + $dir[1];
                if ($nx >= 0 && $nx < $n && $ny >= 0 && $ny < $n && $dormitory[$nx][$ny] == '.') {
                    $dormitory[$nx][$ny] = '@';
                    $newInfected[] = [$nx, $ny];
                }
            }
        }
        $infected = $newInfected;
        if (empty($infected)) {
            break; // No more infections possible
        }
    }
    
    $total = 0;
    for ($i = 0; $i < $n; $i++) {
        for ($j = 0; $j < $n; $j++) {
            if ($dormitory[$i][$j] == '@') {
                $total++;
            }
        }
    }
    
    return $total;
}
function main(){
    assert(countFluInfections([
        ['.','.','.','.','#'], 
        ['.','#','.','@','.'], 
        ['.','#','@','.','.'], 
        ['#','.','.','.','.'], 
        ['#','.','.','.','.']], 4) === 16);
    

    assert(countFluInfections([
            ['.','.','.','.','#'], 
            ['.','#','.','@','.'], 
            ['.','#','@','.','.'], 
            ['#','.','.','.','.'], 
            ['#','.','.','.','.']], 1) === 2);
    

    assert(countFluInfections([
                ['.','.','.','.','#'], 
                ['.','#','.','@','.'], 
                ['.','#','@','.','.'], 
                ['#','.','.','.','.'], 
                ['#','.','.','.','.']], 2) === 7);

    assert(countFluInfections([
                    ['.','.','.','.','#'], 
                    ['.','#','.','#','.'], 
                    ['.','#','#','.','.'], 
                    ['#','.','.','.','.'], 
                    ['#','.','.','.','.']], 100) === 0);
}


main();
?>