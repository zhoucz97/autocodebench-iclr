<?php

/**
    * Calculates the k-th term of the Pell sequence modulo 32767.
    * The Pell sequence is defined as follows:
    *   a[1] = 1, a[2] = 2, and a[n] = 2*a[n-1] + a[n-2] for n > 2.
    * This function computes the k-th term of this sequence modulo 32767.
    * Input: An integer $k (1 ≤ $k < 1000000), representing the position in the sequence.
    * Output: Returns the k-th term of the Pell sequence modulo 32767 as an integer.
    * Example:
    *   For $k = 1, the function returns 1.
    *   For $k = 8, the function returns 408.
    */

    function getPellNumberModulo($k){

    $mod = 32767;
    if ($k == 1) {
        return 1 % $mod;
    } elseif ($k == 2) {
        return 2 % $mod;
    }
    
    $a1 = 1 % $mod;
    $a2 = 2 % $mod;
    
    for ($i = 3; $i <= $k; $i++) {
        $next = (2 * $a2 + $a1) % $mod;
        $a1 = $a2;
        $a2 = $next;
    }
    
    return $a2;
}
function main(){
    assert(getPellNumberModulo(1) === 1);
    assert(getPellNumberModulo(8) === 408);
    assert(getPellNumberModulo(3) === 5);
    assert(getPellNumberModulo(5) === 29);
    assert(getPellNumberModulo(10) === 2378);
    assert(getPellNumberModulo(100000) === 27550);
    assert(getPellNumberModulo(999999) === 21600);
    
}