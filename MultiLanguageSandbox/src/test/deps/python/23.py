

def largest_prime_factor(n: int) -> int:
    """
    Find the largest prime factor of a given positive integer.
    
    The integer is assumed to be the product of exactly two distinct prime numbers. 
    The function iterates through potential factors starting from the smallest prime (2) 
    and checks if they are a factor of 'n'. If a factor is found, the function returns 
    the division of 'n' by this factor, which is the larger prime factor. If no factors 
    are found up to the square root of 'n', then 'n' itself is a prime number and is 
    returned as the largest prime factor.
    
    Args:
    n (int): The positive integer to factorize, which is the product of two distinct primes.
    
    Returns:
    int: The larger of the two prime factors of 'n'.
    
    Examples:
    >>> largest_prime_factor(21)
    7
    >>> largest_prime_factor(15)
    5
    """
   
    """
    Find the largest prime factor of a given positive integer.
    
    The integer is assumed to be the product of exactly two distinct prime numbers. 
    The function iterates through potential factors starting from the smallest prime (2) 
    and checks if they are a factor of 'n'. If a factor is found, the function returns 
    the division of 'n' by this factor, which is the larger prime factor. If no factors 
    are found up to the square root of 'n', then 'n' itself is a prime number and is 
    returned as the largest prime factor.
    
    Args:
    n (int): The positive integer to factorize, which is the product of two distinct primes.
    
    Returns:
    int: The larger of the two prime factors of 'n'.
    
    Examples:
    >>> largest_prime_factor(21)
    7
    >>> largest_prime_factor(15)
    5
    """
    for i in range(2, int(math.sqrt(n)) + 1):
        if n % i == 0:
            return n // i
    return n
def test_largest_prime_factor():
    test_cases = [
        # Test case 1: composite number
        {
            'n': 35,  # 5 * 7
            'expected': 7
        },
        # Test case 2: prime number itself
        {
            'n': 29,
            'expected': 29
        },
        # Test case 3: composite number with a square of a prime
        {
            'n': 49,  # 7 * 7
            'expected': 7
        },
        # Test case 4: large composite number
        {
            'n': 2 * 2 * 3 * 3 * 7 * 11,  # prime factors are 2, 3, 7, 11
            'expected': 11
        },
    ]

    for i, test in enumerate(test_cases, 1):
        result = largest_prime_factor(test['n'])
        assert result == test['expected'], f"Test case {i} failed: result {result} does not match expected {test['expected']}"
        print(f"Test case {i} passed with result {result}.")

# Run the test function
test_largest_prime_factor()