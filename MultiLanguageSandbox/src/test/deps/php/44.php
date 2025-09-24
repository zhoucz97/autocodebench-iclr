<?php

/**
 * Compares the area of a square and a rectangle to determine which is larger.
 * The square's side length is represented by $a, while the rectangle's dimensions are represented by $b and $c.
 * Input: Three integers $a, $b, and $c, where $a is the side length of the square, and $b and $c are the width and height of the rectangle, respectively.
 * Output: Returns a string "Alice" if the square's area is greater, "Bob" if the rectangle's area is greater, or "Equal" if both areas are the same.
 */
function compareArea($a, $b, $c): string {

    $squareArea = $a * $a;
    $rectangleArea = $b * $c;
    
    if ($squareArea > $rectangleArea) {
        return "Alice";
    } elseif ($rectangleArea > $squareArea) {
        return "Bob";
    } else {
        return "Equal";
    }
}
function main(){
    // Test cases
    assert(compareArea(5, 4, 6) === "Alice"); 
    assert(compareArea(7, 5, 10) === "Bob"); 
    assert(compareArea(6, 3, 12) === "Equal"); 

}


main();
?>