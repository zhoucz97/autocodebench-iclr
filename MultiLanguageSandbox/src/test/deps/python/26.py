

from math import gcd
def count_coprime_pairs(x0: int, y0: int) -> int:
    """
    Counts the number of pairs of positive integers (P, Q) such that P and Q have x0 as their greatest common divisor (GCD)
    and y0 as their least common multiple (LCM).

    Args:
    x0 (int): The GCD that each pair (P, Q) must have.
    y0 (int): The LCM that each pair (P, Q) must have.

    Returns:
    int: The count of all possible pairs (P, Q) that meet the specified criteria.

    The function iterates through all possible factors 'a' of the quotient 'y0 / x0'. For each factor 'a',
    it calculates the corresponding factor 'b' such that 'a * b = y0 / x0'. It then checks if 'a' and 'b'
    are co-prime (their GCD is 1) because P and Q can only be multiples of 'x0' by co-prime factors to maintain
    the GCD as 'x0'. If 'a' and 'b' are co-prime, the count is incremented by 1 if 'a' is the same as 'b',
    else by 2 to account for both (P, Q) and (Q, P) pairs.

    Example Cases:

    >>> count_coprime_pairs(3, 60)
    4
    >>> count_coprime_pairs(2, 50)
    0
    """
    if y0 % x0 != 0:
        return 0
    k = y0 // x0
    count = 0
    # Iterate through all possible factors a of k
    for a in range(1, int(math.isqrt(k)) + 1):
        if k % a == 0:
            b = k // a
            if math.gcd(a, b) == 1:
                if a == b:
                    count += 1
                else:
                    count += 2
    return count
def test_count_coprime_pairs():
    test_cases = [
        (3, 60, 4),   # The example from the problem statement
        (2, 50, 2), 
        (6, 180, 8),  # 180/6 = 30, factors are (1, 30), (2, 15), (3, 10), (5, 6), (6, 5), (10, 3), (15, 2), (30, 1)
    ]

    for i, (x0, y0, expected) in enumerate(test_cases):
        result = count_coprime_pairs(x0, y0)
        assert result == expected, f"Test case {i+1} failed: expected {expected}, got {result}"
        print(f"Test case {i+1} passed: x0 = {x0}, y0 = {y0}, expected = {expected}, got = {result}")

# Call the test function
test_count_coprime_pairs()