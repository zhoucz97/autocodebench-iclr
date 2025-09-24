

from typing import List, Tuple
def process_random_numbers(numbers: List[int]) -> Tuple[int, List[int]]:
    """
    Remove duplicate integers from the input list and return the sorted unique numbers.

    The function first converts the input list of numbers into a set to remove any duplicates,
    then sorts the resulting set of unique numbers. It returns a tuple containing the count
    of unique numbers and a list of these unique numbers in ascending order.

    Args:
    numbers (List[int]): A list of integers, which may contain duplicates.

    Returns:
    Tuple[int, List[int]]: A tuple with two elements:
        - An integer representing the count of unique numbers.
        - A list of the unique numbers sorted in ascending order.

    Examples:
    >>> process_random_numbers([20, 40, 32, 67, 40, 20, 89, 300, 400, 15])
    (8, [15, 20, 32, 40, 67, 89, 300, 400])

    >>> process_random_numbers([5, 3, 5, 2, 1, 4, 1])
    (5, [1, 2, 3, 4, 5])
    """
    # Convert the list to a set to remove duplicates
    unique_numbers = set(numbers)
    
    # Sort the unique numbers
    sorted_unique_numbers = sorted(unique_numbers)
    
    # Return the count of unique numbers and the sorted list
    return (len(sorted_unique_numbers), sorted_unique_numbers)
def test_process_random_numbers():
    # Test case 1: Basic example with duplicates
    assert process_random_numbers([20, 40, 32, 67, 40, 20, 89, 300, 400, 15]) == (8, [15, 20, 32, 40, 67, 89, 300, 400]), "Test case 1 failed"

    # Test case 2: All elements are the same
    assert process_random_numbers([1, 1, 1, 1]) == (1, [1]), "Test case 2 failed"

    # Test case 3: No duplicates
    assert process_random_numbers([3, 1, 4, 2]) == (4, [1, 2, 3, 4]), "Test case 3 failed"

    # Test case 4: Random numbers with single element
    assert process_random_numbers([42]) == (1, [42]), "Test case 4 failed"

    # Test case 5: Empty list
    assert process_random_numbers([]) == (0, []), "Test case 5 failed"

    # Test case 6: Random numbers with negatives and zero
    assert process_random_numbers([0, -1, -2, -1, 0, 3]) == (4, [-2, -1, 0, 3]), "Test case 6 failed"

    # Test case 7: Large range of numbers
    assert process_random_numbers(list(range(1000, 0, -1))) == (1000, list(range(1, 1001))), "Test case 7 failed"

    print("All test cases passed!")

# Run the test function
test_process_random_numbers()