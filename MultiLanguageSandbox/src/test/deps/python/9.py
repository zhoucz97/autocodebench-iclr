

from itertools import permutations

def find_number_combinations():
    """
    Generate all unique combinations of three numbers, each formed from the digits 1 to 9 without repetition,
    such that the second number is twice the first and the third is three times the first.

    Returns:
        list of tuples: A sorted list of tuples, where each tuple contains three integers representing the
                        valid number combinations in ascending order based on the first number.

    Example:
        >>> find_number_combinations()
        [(123, 246, 369), (124, 248, 372), ...]
    """
    digits = '123456789'
    result = []
    
    # We need to check all possible lengths for A, B, and C such that:
    # len(A) + len(B) + len(C) <= 9, and all digits are distinct across A, B, C.
    # Since B = 2*A and C = 3*A, the digits of B and C must be a permutation of some subset of the remaining digits.
    
    # The approach is to generate all possible A's (as numbers formed by distinct digits), then compute B and C,
    # and check if the digits in B and C are distinct from A's digits and among themselves.
    
    # Iterate over all possible lengths for A (from 1 to 9)
    for length_a in range(1, 10):
        # Generate all possible permutations of 'length_a' distinct digits for A
        for perm in permutations(digits, length_a):
            a = int(''.join(perm))
            b = 2 * a
            c = 3 * a
            
            # Convert B and C to strings to check their digits
            str_b = str(b)
            str_c = str(c)
            
            # Check if the digits in B and C are distinct from each other and from A's digits
            all_digits = set(perm) | set(str_b) | set(str_c)
            if len(all_digits) == (length_a + len(str_b) + len(str_c)):
                # Also, ensure that B and C are formed by digits not in A, and among themselves no repeats
                # The above condition checks that all digits combined are distinct, but we also need to ensure
                # that the digits of B and C are within 1-9 and not reused in A.
                # So, the digits of B and C must be a subset of the remaining digits not in A.
                remaining_digits = set(digits) - set(perm)
                if set(str_b).issubset(remaining_digits) and set(str_c).issubset(remaining_digits):
                    # Additionally, B and C should not share any digits among themselves
                    if set(str_b).isdisjoint(set(str_c)):
                        result.append((a, b, c))
    
    # Sort the result based on the first number in each tuple
    result.sort()
    return result
def test_find_number_combinations():
    # Call the function to get the combinations
    combinations = find_number_combinations()

    # Check that we have at least one valid combination
    assert len(combinations) > 0, "There should be at least one valid combination."

    # Iterate over each combination to perform further checks
    for combo in combinations:
        # Each combination should have exactly three numbers
        assert len(combo) == 3, "Each combination should have three numbers."

        # Check if numbers are 3-digit numbers
        for num in combo:
            assert 100 <= num <= 999, f"Each number should be a 3-digit number, got {num}."

        # Check the 1:2:3 ratio
        assert combo[1] == 2 * combo[0] and combo[2] == 3 * combo[0], "The numbers should be in a 1:2:3 ratio."

    print("All test cases passed!")