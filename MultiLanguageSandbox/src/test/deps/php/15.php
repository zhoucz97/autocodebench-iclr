<?php


/**
 * Finds the rectangle with the largest area that includes a given point.
 * In this task, we are given a list of rectangles, each defined by two opposite corner points,
 * and a point referred to as the "needle". The rectangles' sides are parallel to the coordinate axes.
 * The goal is to find the rectangle with the maximum area that contains the needle (including its boundary).
 * Input: An array of rectangles (each rectangle is an array of two points [x1, y1, x2, y2]),
 *        and a needle point [x, y].
 * Output: Returns the index (1-based) of the rectangle with the largest area that contains the needle.
 * Example: For rectangles [[1,1,4,4], [2,2,6,6]] and needle point [3,3],
 *          the function should return 2, as the second rectangle is larger and contains the needle.
 */


function findMaxAreaRectangle($rectangles, $needle){

    $maxArea = -1;
    $resultIndex = -1;
    $needleX = $needle[0];
    $needleY = $needle[1];
    
    foreach ($rectangles as $index => $rectangle) {
        // Extract rectangle coordinates
        $x1 = $rectangle[0];
        $y1 = $rectangle[1];
        $x2 = $rectangle[2];
        $y2 = $rectangle[3];
        
        // Determine which point is the bottom-left and which is the top-right
        $minX = min($x1, $x2);
        $maxX = max($x1, $x2);
        $minY = min($y1, $y2);
        $maxY = max($y1, $y2);
        
        // Check if needle is inside this rectangle (including boundary)
        if ($needleX >= $minX && $needleX <= $maxX && $needleY >= $minY && $needleY <= $maxY) {
            // Calculate area
            $width = $maxX - $minX;
            $height = $maxY - $minY;
            $area = $width * $height;
            
            // Update result if this rectangle has larger area
            if ($area > $maxArea) {
                $maxArea = $area;
                $resultIndex = $index + 1; // Convert to 1-based index
            }
        }
    }
    
    return $resultIndex;
}
function main(){
    assert(findMaxAreaRectangle([[2,3,6,8], [1,3,5,6], [8, 2, 1, 9]], [4,4]) === 3);
    assert(findMaxAreaRectangle([[6,8,2,3], [1,3,5,6], [8, 2, 1, 9]], [4,4]) === 3);
    assert(findMaxAreaRectangle([[1,1,4,4], [2,2,6,6]], [3,3]) === 2);
    assert(findMaxAreaRectangle([[0,0,5,5], [1,1,3,3]], [2,2]) === 1);
    assert(findMaxAreaRectangle([[1,1,2,2], [2,2,4,4]], [5,5]) === -1); // Needle is outside all rectangles
    
}


main();
?>