
/**
 * You are given an array a of n integers. You can perform at most one operation where
 * you select three integers i, j, x (1 <= i <= j <= n) and replace all elements from a[i] to a[j]
 * with x, at a cost of (j - i + 1).
 * Find the minimum cost to make all elements in the array equal.
 */

function makeEqualAgain(n: number, a: number[]): number {
    // Check if all elements are already the same
    let allSame = true;
    for (let i = 1; i < n; i++) {
        if (a[i] !== a[0]) {
            allSame = false;
            break;
        }
    }
    if (allSame) {
        return 0;
    }

    // The minimal cost is the minimal between:
    // 1. Replacing the entire array (cost n)
    // 2. Replacing a segment such that the remaining elements are uniform
    // The optimal segment is the one where the left and right parts are the same, and the segment to replace is the minimal possible to make all elements equal to that value.

    // The idea is to find the leftmost and rightmost positions where the value is the same as the first or last element, and the segment between them can be replaced.
    // Alternatively, the minimal cost is the minimal between:
    // - The cost to make all elements equal to the first element (replace the suffix that differs)
    // - The cost to make all elements equal to the last element (replace the prefix that differs)
    // - Other possibilities, but these two cover the main cases.

    // Option 1: make all elements equal to a[0]
    let cost1 = 0;
    let i = 0;
    while (i < n && a[i] === a[0]) {
        i++;
    }
    if (i < n) {
        cost1 = n - i; // replace from i to n-1
    }

    // Option 2: make all elements equal to a[n-1]
    let cost2 = 0;
    let j = n - 1;
    while (j >= 0 && a[j] === a[n - 1]) {
        j--;
    }
    if (j >= 0) {
        cost2 = j + 1; // replace from 0 to j
    }

    // The minimal cost is the minimal between cost1, cost2, and n (replacing the entire array)
    return Math.min(cost1, cost2, n);
}
function testMakeEqualAgain(): void {
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

};

testMakeEqualAgain();