
/**
 * You are given an array a of n integers. You can perform at most one operation where
 * you select three integers i, j, x (1 <= i <= j <= n) and replace all elements from a[i] to a[j]
 * with x, at a cost of (j - i + 1).
 * Find the minimum cost to make all elements in the array equal.
 */

function makeEqualAgain(n, a) {
    if (n === 1) return 0; // Only one element, already equal
    
    let left = 0;
    while (left < n && a[left] === a[0]) {
        left++;
    }
    
    if (left === n) return 0; // All elements are the same
    
    let right = n - 1;
    while (right >= 0 && a[right] === a[n - 1]) {
        right--;
    }
    
    // The minimal cost is the smaller between replacing left part or right part
    const costLeft = left; // replace from 0 to left-1 (length left)
    const costRight = n - 1 - right; // replace from right+1 to n-1 (length n - 1 - right)
    
    return Math.min(costLeft, costRight);
}
function testMakeEqualAgain() {
    console.assert(makeEqualAgain(6, [1, 2, 3, 4, 5, 1]) === 4);
    console.assert(makeEqualAgain(7, [1, 1, 1, 1, 1, 1, 1]) === 0);
    console.assert(makeEqualAgain(8, [8, 8, 8, 1, 2, 8, 8, 8]) === 2);
    console.assert(makeEqualAgain(3, [1, 2, 3]) === 2);
    console.assert(makeEqualAgain(7, [4, 3, 2, 7, 1, 1, 3]) === 6);
    console.assert(makeEqualAgain(9, [9, 9, 2, 9, 2, 5, 5, 5, 3]) === 7);
    
    console.assert(makeEqualAgain(3, [1, 2, 1]) === 1);
    console.assert(makeEqualAgain(5, [5, 5, 1, 5, 5]) === 1);
    console.assert(makeEqualAgain(4, [1, 1, 1, 1]) === 0);
    console.assert(makeEqualAgain(6, [2, 2, 2, 3, 2, 2]) === 1);
    console.assert(makeEqualAgain(1, [1]) === 0);
    console.assert(makeEqualAgain(2, [1, 2]) === 1);
    console.assert(makeEqualAgain(4, [1, 2, 2, 1]) === 2);
    console.assert(makeEqualAgain(7, [4, 4, 4, 3, 3, 4, 4]) === 2);
    console.assert(makeEqualAgain(6, [5, 4, 4, 4, 5, 5]) === 3);
    console.assert(makeEqualAgain(7, [1, 2, 1, 2, 1, 2, 1]) === 5);

}

testMakeEqualAgain();