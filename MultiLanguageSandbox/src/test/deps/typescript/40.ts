
/**
 * Given two positive integers n and k, find the number of positive integers x,
 * where 1 <= x <= n, that can be expressed as x = a^b with a and b being positive integers
 * and b >= k. Different legal representations of the same number are counted only once.
 * Example:
 *   countPowerNumbers(99, 1) returns 99
 */

function countPowerNumbers(n: number, k: number): number {
    const powerNumbers = new Set<number>();
    
    // Iterate over possible bases a
    for (let a = 1; ; ) {
        let power = 1;
        let b = 0;
        // Compute a^b for b >= k until a^b exceeds n
        while (true) {
            b++;
            if (b < k) continue;
            power *= a;
            if (power > n) break;
            powerNumbers.add(power);
        }
        a++;
        if (a > Math.pow(n, 1 / k)) break;
    }
    
    return powerNumbers.size;
}
const testCountPowerNumbers = (): void => {
    console.assert(countPowerNumbers(99, 1) === 99, `Expected 99, got ${countPowerNumbers(99, 1)}`);
    console.assert(countPowerNumbers(99, 3) === 7, `Expected 7, got ${countPowerNumbers(99, 3)}`);
    console.assert(countPowerNumbers(99, 2) === 12, `Expected 12, got ${countPowerNumbers(99, 2)}`);
    console.assert(countPowerNumbers(10, 1) === 10, `Expected 10, got ${countPowerNumbers(10, 1)}`);
    console.assert(countPowerNumbers(10, 2) === 4, `Expected 4, got ${countPowerNumbers(10, 2)}`);
    console.assert(countPowerNumbers(500, 1) === 500, `Expected 500, got ${countPowerNumbers(500, 1)}`);
    console.assert(countPowerNumbers(500, 2) === 30, `Expected 30, got ${countPowerNumbers(500, 2)}`);
    console.assert(countPowerNumbers(500, 3) === 13, `Expected 13, got ${countPowerNumbers(500, 3)}`);
    console.assert(countPowerNumbers(1000, 1) === 1000, `Expected 1000, got ${countPowerNumbers(1000, 1)}`);
    console.assert(countPowerNumbers(1000, 2) === 41, `Expected 41, got ${countPowerNumbers(1000, 2)}`);
    console.assert(countPowerNumbers(1000, 3) === 17, `Expected 17, got ${countPowerNumbers(1000, 3)}`);
    console.assert(countPowerNumbers(1000, 93) === 1, `Expected 1, got ${countPowerNumbers(1000, 93)}`);
    console.assert(countPowerNumbers(50, 2) === 10, `Expected 10, got ${countPowerNumbers(50, 2)}`);
    console.assert(countPowerNumbers(50, 3) === 5, `Expected 5, got ${countPowerNumbers(50, 3)}`);
    console.assert(countPowerNumbers(2, 3) === 1, `Expected 1, got ${countPowerNumbers(2, 3)}`);
};

testCountPowerNumbers();