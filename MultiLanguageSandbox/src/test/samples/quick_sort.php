<?php

function quicksort($list) {
    if ($list === null || count($list) <= 1) {
        return $list ?? [];
    }
    
    $pivot = $list[0];
    $rest = array_slice($list, 1);
    $less = array_filter($rest, function($x) use ($pivot) {
        return $x < $pivot;
    });
    $greater = array_filter($rest, function($x) use ($pivot) {
        return $x >= $pivot;
    });
    
    return array_merge(quicksort(array_values($less)), [$pivot], quicksort(array_values($greater)));
}

function checkQuicksort() {
    assert(quicksort([3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]) === [1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 9]);
    assert(quicksort([5, 4, 3, 2, 1]) === [1, 2, 3, 4, 5]);
    assert(quicksort([]) === []);
    assert(quicksort([1]) === [1]);
}

checkQuicksort();
