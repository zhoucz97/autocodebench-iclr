

def verify_isbn(isbn: str) -> str:
    """
    Verify the correctness of a given ISBN number and correct it if necessary.

    The function checks the provided ISBN number against the ISBN standard checksum calculation.
    If the checksum is correct, the function returns "Right". If the checksum is incorrect,
    the function returns the corrected ISBN number.

    Args:
    isbn: A string representing the ISBN number to be verified. The format should be 'x-xxx-xxxxx-x',
          where 'x' is a digit, and the last 'x' could also be 'X' representing the checksum digit.

    Returns:
    A string that is either "Right" if the ISBN checksum is correct, or the corrected ISBN number
    in the same format as the input if the checksum is incorrect.

    Examples:
    >>> verify_isbn("0-670-82162-4")
    'Right'
    
    >>> verify_isbn("0-670-82162-0")
    '0-670-82162-4'
    """
    # Split the ISBN into parts based on hyphens
    parts = isbn.split('-')
    if len(parts) != 4:
        # This case shouldn't happen per problem statement, but handle just in case
        return "Invalid format"
    
    # Combine all parts except the last one (checksum part)
    combined = ''.join(parts[:-1])
    checksum_char = parts[-1]
    
    if len(combined) != 9:
        return "Invalid format"
    
    # Process each character in the combined string (first 9 digits)
    total = 0
    for i in range(9):
        char = combined[i]
        if not char.isdigit():
            return "Invalid format"
        digit = int(char)
        total += digit * (i + 1)  # positions are 1-based
    
    # Calculate the checksum digit
    mod = total % 11
    correct_checksum = 'X' if mod == 0 else str(11 - mod)
    
    # Compare with the provided checksum
    if checksum_char.upper() == correct_checksum:
        return "Right"
    else:
        # Construct the corrected ISBN
        corrected_isbn = f"{parts[0]}-{parts[1]}-{parts[2]}-{correct_checksum}"
        return corrected_isbn
def test_verify_isbn():
    # Test case 1: Correct ISBN number
    assert verify_isbn("0-670-82162-4") == "Right", "Test case 1 failed"

    # Test case 2: Incorrect ISBN number with wrong checksum digit
    assert verify_isbn("0-670-82162-0") == "0-670-82162-4", "Test case 2 failed"

    print("All test cases passed!")

# Run the test cases
test_verify_isbn()