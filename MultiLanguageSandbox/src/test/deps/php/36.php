<?php


/**
 * Evaluate a Polish expression and return the result.
 * In Polish notation, operators precede their operands. For example, the expression "+ 11.0 12.0" is equivalent to "11.0 + 12.0".
 * Supported operators are +, -, *, and /.
 * Input: A string containing a valid Polish expression separated by spaces.
 * Output: A float representing the result of evaluating the expression.
 * 
 * Example:
 * - For the input "+ 11.0 12.0", the function should return 23.0.
 * - For the input "* + 2.0 3.0 4.0", the function should return 20.0.
 */


 function evaluatePolishExpression($expression) {

    $tokens = explode(' ', $expression);
    $stack = array();
    
    // Process tokens in reverse order
    for ($i = count($tokens) - 1; $i >= 0; $i--) {
        $token = $tokens[$i];
        if (is_numeric($token)) {
            array_push($stack, (float)$token);
        } else {
            $operand1 = array_pop($stack);
            $operand2 = array_pop($stack);
            switch ($token) {
                case '+':
                    $result = $operand1 + $operand2;
                    break;
                case '-':
                    $result = $operand1 - $operand2;
                    break;
                case '*':
                    $result = $operand1 * $operand2;
                    break;
                case '/':
                    $result = $operand1 / $operand2;
                    break;
            }
            array_push($stack, $result);
        }
    }
    
    return array_pop($stack);
}
function main(){
    assert(evaluatePolishExpression("* + 11.0 12.0 + 24.0 35.0") === 1357.0);
    assert(evaluatePolishExpression("+ 2.0 2.0") === 4.0);
    assert(evaluatePolishExpression("- 10.0 5.0") === 5.0);
    // Testcase 1
    $expression1 = "+ 11.0 12.0";
    $result1 = evaluatePolishExpression($expression1);
    assert($result1 === 23.0);

    // Testcase 2
    $expression2 = "* + 2.0 3.0 4.0";
    $result2 = evaluatePolishExpression($expression2);
    assert($result2 === 20.0);



}

main();
?>