<?php


/**
    * Finds the fractional element in a diagonally arranged sequence.
    * In this sequence, fractions are arranged in a diagonal pattern. The first row contains fractions with the numerator 1 and increasing denominators. Each subsequent row increases the numerator and decreases the denominator until it reaches 1.
    * For example, the sequence starts with 1/1, 1/2, 2/1, 3/1, 2/2, ...
        1/1   1/2   1/3   1/4   1/5

        2/1   2/2   2/3   2/4

        3/1   3/2   3/3

        4/1   4/2

        5/1

    * The function takes an integer 'n' and returns the 'n'th element in this sequence as a string in the form 'numerator/denominator'.
    * Input: An integer 'n' (n < 20000).
    * Output: A string representing the 'n'th element in the sequence.
    * Examples:
    * - Input: 3, Output: "2/1"
    * - Input: 14, Output: "2/4"
*/


function findFractionalElement($n) {

    $k = 1;
    while ($k * ($k + 1) / 2 < $n) {
        $k++;
    }
    $totalBefore = ($k - 1) * $k / 2;
    $posInDiagonal = $n - $totalBefore;
    
    $numerator = $posInDiagonal;
    $denominator = $k - $posInDiagonal + 1;
    
    return "$numerator/$denominator";
}
function main(){
    assert(findFractionalElement(3) === "2/1");
    assert(findFractionalElement(14) === "2/4");
    assert(findFractionalElement(7) === "1/4");
    assert(findFractionalElement(12345) === "59/99");
    
}

main();
?>