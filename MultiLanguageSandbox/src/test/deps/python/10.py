

def sum_of_factorials_optimized(n: int) -> int:
    """
    Compute the cumulative sum of factorial values from 1 up to a specified integer n.

    This function employs a more efficient method to calculate factorials by iteratively
    multiplying the current number to the previous factorial result, thus avoiding redundant
    calculations. It then accumulates the factorial of each number from 1 to n inclusively.

    Args:
    n (int): The upper limit of the range for which the factorial sum is to be calculated.
             The value of n should be in the range of 1 to 50, inclusive.

    Returns:
    int: The sum of all factorial values from 1! to n!.

    Examples:
    >>> sum_of_factorials_optimized(3)
    9
    >>> sum_of_factorials_optimized(5)
    153
    """
    if n < 1 or n > 50:
        raise ValueError("n must be in the range 1 to 50 inclusive")
    
    total_sum = 0
    current_factorial = 1
    
    for i in range(1, n + 1):
        current_factorial *= i
        total_sum += current_factorial
    
    return total_sum
def test_sum_of_factorials_optimized():
    assert sum_of_factorials_optimized(1) == 1, "Test case 1 failed: sum_of_factorials_optimized(1) should be 1."
    assert sum_of_factorials_optimized(3) == 9, "Test case 2 failed: sum_of_factorials_optimized(3) should be 9."
    assert sum_of_factorials_optimized(4) == 33, "Test case 3 failed: sum_of_factorials_optimized(4) should be 33."
    assert sum_of_factorials_optimized(5) == 153, "Test case 4 failed: sum_of_factorials_optimized(5) should be 153."
    assert sum_of_factorials_optimized(6) == 873, "Test case 5 failed: sum_of_factorials_optimized(6) should be 873."
    assert sum_of_factorials_optimized(10) == 4037913, "Test case 6 failed: sum_of_factorials_optimized(10) should be 4037913."
    print("All test cases passed.")

# Call the test function to run the test cases
test_sum_of_factorials_optimized()