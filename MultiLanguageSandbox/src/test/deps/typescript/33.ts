
/**
 * This problem introduces a new modulo operation, denoted by the symbol "⊕".
 * When calculating x ⊕ y, if x is not a multiple of y, the result is the remainder of x divided by y.
 * Otherwise, divide x by y repeatedly until x is no longer a multiple of y, denoting the final value as x'.
 * Then the result is the remainder of x' divided by y.
 * For example, 4⊕5=4, 20⊕5=4, 100⊕5=4.
 * Given a prime number p, there will be several queries. For each query, an integer n is given,
 * and you need to compute the value of n! ⊕ p, where n! is the factorial of n,
 * i.e. the product of all positive integers less than or equal to n.
 * 
 * @example
 * newModuloFactorial(3, 7); // returns 6
 */

function newModuloFactorial(n: number, p: number): number {
    let factorial = 1;
    for (let i = 1; i <= n; i++) {
        factorial *= i;
    }
    
    // Check if factorial is a multiple of p
    if (factorial % p !== 0) {
        return factorial % p;
    } else {
        // Divide factorial by p until it's no longer a multiple of p
        let temp = factorial;
        while (temp % p === 0) {
            temp = Math.floor(temp / p);
        }
        return temp % p;
    }
}
(function testNewModuloFactorial(): void {
    console.assert(newModuloFactorial(3, 7) === 6, 'Test at n=3, p=7 failed');
    console.assert(newModuloFactorial(10, 3) === 1, 'Test at n=10, p=3 failed');
    console.assert(newModuloFactorial(11, 7) === 4, 'Test at n=11, p=7 failed');
    console.assert(newModuloFactorial(45, 7) === 1, 'Test at n=45, p=7 failed');
    console.assert(newModuloFactorial(14, 7) === 2, 'Test at n=14, p=7 failed');
    console.assert(newModuloFactorial(1919, 10007) === 3152, 'Test at n=1919, p=10007 failed');
    console.assert(newModuloFactorial(810, 10007) === 3679, 'Test at n=810, p=10007 failed');
    console.assert(newModuloFactorial(1, 2) === 1, 'Test at n=1, p=2 failed');
    console.assert(newModuloFactorial(5, 11) === 10, 'Test at n=5, p=11 failed');
    console.assert(newModuloFactorial(6, 13) === 5, 'Test at n=6, p=13 failed');
    console.assert(newModuloFactorial(8, 17) === 13, 'Test at n=8, p=17 failed');
    console.assert(newModuloFactorial(15, 19) === 16, 'Test at n=15, p=19 failed');
    console.assert(newModuloFactorial(21, 23) === 1, 'Test at n=21, p=23 failed');
    console.assert(newModuloFactorial(30, 29) === 28, 'Test at n=30, p=29 failed');
    console.assert(newModuloFactorial(100, 101) === 100, 'Test at n=100, p=101 failed');
})();