<?php

/**
 * Generates all possible ways to express a natural number as the sum of smaller natural numbers.
 * For example, when n=7, there are 14 different ways to express it as the sum of smaller natural numbers.
 * This function returns all these expressions in lexicographical (dictionary) order.
 * below is output when n = 7
 *  7=1+1+1+1+1+1+1
    7=1+1+1+1+1+2
    7=1+1+1+1+3
    7=1+1+1+2+2
    7=1+1+1+4
    7=1+1+2+3
    7=1+1+5
    7=1+2+2+2
    7=1+2+4
    7=1+3+3
    7=1+6
    7=2+2+3
    7=2+5
    7=3+4
 * @param int $n The input natural number (greater than 1).
 * @return array An array of strings representing all possible expressions.
 */

 function generateNumberExpressions($n) {

    $result = array();
    backtrack($n, 1, array(), $result);
    return $result;
}

function backtrack($remaining, $start, $current, &$result) {
    if ($remaining == 0) {
        $expression = implode('+', $current);
        $result[] = $expression;
        return;
    }
    
    for ($i = $start; $i <= $remaining; $i++) {
        $newCurrent = $current;
        $newCurrent[] = $i;
        backtrack($remaining - $i, $i, $newCurrent, $result);
    }
}
function main(){
    $result = generateNumberExpressions(7);
    assert($result===["7=1+1+1+1+1+1+1","7=1+1+1+1+1+2","7=1+1+1+1+3","7=1+1+1+2+2","7=1+1+1+4","7=1+1+2+3","7=1+1+5","7=1+2+2+2","7=1+2+4","7=1+3+3","7=1+6","7=2+2+3","7=2+5","7=3+4"]);
    $result = generateNumberExpressions(6);
    assert($result===[  "6=1+1+1+1+1+1","6=1+1+1+1+2","6=1+1+1+3","6=1+1+2+2","6=1+1+4","6=1+2+3","6=1+5","6=2+2+2","6=2+4","6=3+3"]);
  
  
}


main();
?>