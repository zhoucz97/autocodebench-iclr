<?php

/**
    * This function calculates the minimum number of 6x6xh packages required to ship an order of products of various sizes.
    * The sizes of the products range from 1x1 to 6x6, and each product requires a certain amount of space in the package.
    * The goal is to efficiently pack these products to minimize the number of packages, reducing shipping costs.
    *
    * Input: Six integers ($a, $b, $c, $d, $e, $f), each representing the quantity of products from size 1x1 to 6x6.
    * Output: An integer representing the minimum number of 6x6xh packages required to ship the order.
    *
    * Example:
    * For an order with 1 product of size 1x1, 2 products of size 2x2, and 1 product of size 3x3,
    * the function will calculate and return the minimum number of packages needed.
    */

function calculateMinimumPackages($a, $b, $c, $d, $e, $f): int {

    $packages = 0;
    $sizes = [6 => $f, 5 => $e, 4 => $d, 3 => $c, 2 => $b, 1 => $a];
    
    foreach ($sizes as $size => $count) {
        if ($count <= 0) continue;
        
        $area = $size * $size;
        $remaining = 36 - $area;
        
        // Try to fit the current product in existing packages
        for ($i = 0; $i < $packages; $i++) {
            if ($remaining >= 0 && $count > 0) {
                $maxPossible = min(floor($remaining / ($size * $size)), $count);
                if ($maxPossible > 0) {
                    $count -= $maxPossible;
                    $remaining -= $maxPossible * $area;
                }
            }
        }
        
        // If there are still products left, create new packages
        if ($count > 0) {
            $packages += $count;
        }
    }
    
    return $packages;
}
function main(){
    assert(calculateMinimumPackages(0, 0, 4, 0, 0, 1) === 2);
    assert(calculateMinimumPackages(7, 5, 1, 0, 0, 0) === 1);
    assert(calculateMinimumPackages(0, 0, 0, 0, 0, 0) === 0);
    assert(calculateMinimumPackages(0, 0, 4, 1, 1, 1) === 4);
    assert(calculateMinimumPackages(1, 2, 1, 1, 1, 1) === 4);
    
}


main();
?>