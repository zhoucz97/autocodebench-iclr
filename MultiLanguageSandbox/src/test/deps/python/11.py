

def cantor_numbering(N: int) -> str:
    """
    Determines the numerator and denominator of the N-th term in the Cantor table.

    Args:
    - N (int): The position in the Cantor table to find the corresponding fraction for.

    Returns:
    - str: A string representation of the fraction at the N-th position in the Cantor table.
    """
    # Find the diagonal k where the N-th term is located
    k = math.ceil((math.sqrt(8 * N + 1) - 1) / 2)
    
    # Calculate the position within the diagonal
    pos_in_diagonal = N - (k * (k - 1)) // 2
    
    if k % 2 == 1:
        # Odd diagonal: numerator increases, denominator decreases
        numerator = pos_in_diagonal
        denominator = k - pos_in_diagonal + 1
    else:
        # Even diagonal: numerator decreases, denominator increases
        numerator = k - pos_in_diagonal + 1
        denominator = pos_in_diagonal
    
    return f"{numerator}/{denominator}"
def test_cantor_numbering():
    # Test case 1: N = 1, the first element, should be "1/1"
    assert cantor_numbering(1)=="1/1"

    # Test case 2: N = 2, the second element, should be "1/2"
    assert cantor_numbering(2)=="1/2"

    # Test case 3: N = 6, the sixth element, should be "1/3"
    assert cantor_numbering(6)=="1/3"

    # Corrected test case: N = 7, the seventh element, should be "1/4"
    assert cantor_numbering(7) == "1/4"

    # Additional test case: N = 8, the eighth element, should be "2/3"
    assert cantor_numbering(8) == "2/3"


test_cantor_numbering()