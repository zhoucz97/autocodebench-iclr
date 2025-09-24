<?php
/**
 * Question Description:
You are given a string consisting of distinct lowercase letters. Your task is to write a function generatePermutations that generates all possible permutations of the input string and returns them in lexicographical order.

Input:
A string consisting of distinct lowercase letters, where the length of the string is at most 6.

Output:
An array containing all permutations of the input string, sorted in lexicographical order.

Example:
    $permutations = generatePermutations("abc");
    $permutations should contain the following array:
    ["abc", "acb", "bac", "bca", "cab", "cba"]
 */


 function generatePermutations($str) {

    $result = [];
    $length = strlen($str);
    
    // Helper function to generate permutations recursively
    function permute($current, $remaining, &$result) {
        if (strlen($remaining) == 0) {
            $result[] = $current;
            return;
        }
        
        for ($i = 0; $i < strlen($remaining); $i++) {
            $newCurrent = $current . $remaining[$i];
            $newRemaining = substr($remaining, 0, $i) . substr($remaining, $i + 1);
            permute($newCurrent, $newRemaining, $result);
        }
    }
    
    permute("", $str, $result);
    
    // Sort the result lexicographically
    sort($result);
    
    return $result;
}

// Example usage:
$permutations = generatePermutations("abc");
print_r($permutations);
function main(){
    $permutations = generatePermutations("abc");
    assert($permutations === ["abc", "acb", "bac", "bca", "cab", "cba"]);
    $permutations = generatePermutations("abcd");
    assert($permutations === ["abcd","abdc","acbd","acdb","adbc","adcb","bacd","badc","bcad","bcda","bdac","bdca","cabd","cadb","cbad","cbda","cdab","cdba","dabc","dacb","dbac","dbca","dcab","dcba"]);

}


main();
?>