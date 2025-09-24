
--[[
This problem introduces a new modulo operation, denoted by the symbol "⊕". When calculating x ⊕ y,
if x is not a multiple of y, the result is the remainder of x divided by y. Otherwise, divide x by y
repeatedly until x is no longer a multiple of y, denoting the final value as x'. Then the result is
the remainder of x' divided by y. For example, 4⊕5=4, 20⊕5=4, 100⊕5=4. Given a prime number p, there
will be several queries. For each query, an integer n is given, and you need to compute the value of
n! ⊕ p, where n! is the factorial of n, i.e., the product of all positive integers less than or
equal to n.

Examples:
    >>> newModuloFactorial(3, 7)
    6
]]--

function newModuloFactorial(n, p)
    -- Compute n!
    local factorial = 1
    for i = 1, n do
        factorial = factorial * i
    end
    
    -- Handle the case where factorial is 0 (n >= p and p is prime, but since p is prime and n < p, factorial mod p is non-zero)
    -- But for n >= p, factorial is divisible by p, so we need to remove all p factors.
    local x = factorial
    while x % p == 0 do
        x = x / p
    end
    
    return x % p
end
do
    assert(newModuloFactorial(3, 7) == 6)
    assert(newModuloFactorial(10, 3) == 1)
    assert(newModuloFactorial(11, 7) == 4)
    assert(newModuloFactorial(45, 7) == 1)
    assert(newModuloFactorial(14, 7) == 2)
    assert(newModuloFactorial(1919, 10007) == 3152)
    assert(newModuloFactorial(810, 10007) == 3679)
    assert(newModuloFactorial(3, 7) == 6)
    assert(newModuloFactorial(10, 3) == 1)
    assert(newModuloFactorial(1, 2) == 1)
    assert(newModuloFactorial(5, 11) == 10)
    assert(newModuloFactorial(6, 13) == 5)
    assert(newModuloFactorial(8, 17) == 13)
    assert(newModuloFactorial(15, 19) == 16)
    assert(newModuloFactorial(21, 23) == 1)
    assert(newModuloFactorial(30, 29) == 28)
    assert(newModuloFactorial(100, 101) == 100)
end