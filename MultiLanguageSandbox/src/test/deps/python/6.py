

def add(a: int, b: int) -> int:
    """
    Adds two integer numbers together.

    Parameters:
    a (int): The first number to be added.
    b (int): The second number to be added.

    Returns:
    int: The sum of the two numbers.
    """
    """
    Adds two integer numbers together.

    Parameters:
    a (int): The first number to be added.
    b (int): The second number to be added.

    Returns:
    int: The sum of the two numbers.
    """
    return a + b
# Test cases:
# The following lines are not part of the function's implementation but are provided
# for testing purposes.

# Test case 1: Positive integers
assert add(2, 3) == 5

# Test case 2: A positive and a negative integer
assert add(-1, 1) == 0

# Test case 3: Two negative integers
assert add(-4, -5) == -9

# Test case 4: Zeroes
assert add(0, 0) == 0

# Test case 5: Large integers
assert add(123456, 654321) == 777777

print("All test cases passed.")