

def max_product(num_str: str, N: int, K: int) -> int:
    """
    Function name: max_product
    Arguments:
    num_str (str): A string representing the number string.
    N (int): An integer representing the length of the number string.
    K (int): An integer representing the number of multiplication signs to insert.
    Return type: int (The function returns the maximum product that can be obtained by inserting Kmultiplication signs into the number string.)
    """
    # Initialize a DP table where dp[i][j] represents the maximum product for first i characters with j multiplications
    dp = [[-1 for _ in range(K+1)] for _ in range(N+1)]
    
    # Base case: no multiplications, the product is the number itself
    for i in range(1, N+1):
        dp[i][0] = int(num_str[:i])
    
    for j in range(1, K+1):
        for i in range(j+1, N+1):  # At least j+1 characters needed for j multiplications (split into j+1 parts)
            max_val = -1
            for m in range(j, i):  # m ranges from j to i-1 to ensure at least one character per segment
                current_segment = int(num_str[m:i])
                if dp[m][j-1] != -1:
                    current_product = dp[m][j-1] * current_segment
                    if current_product > max_val:
                        max_val = current_product
            dp[i][j] = max_val
    
    return dp[N][K]
def test_max_product():
    # Test case 1
    num_str1 = "123"
    N1 = 3
    K1 = 1
    expected_result1 = 36  # Corrected expected result: 1*23 = 23 or 12*3 = 36, max is 36
    assert max_product(num_str1, N1,
                       K1) == expected_result1, f"Test case 1 failed. Expected {expected_result1}, got {max_product(num_str1, N1, K1)}"

    # Test case 2
    num_str2 = "1234"
    N2 = 4
    K2 = 2
    expected_result2 = 144  # Corrected expected result: 1*2*34 = 68 or 12*3*4 = 144, max is 144
    assert max_product(num_str2, N2,
                       K2) == expected_result2, f"Test case 2 failed. Expected {expected_result2}, got {max_product(num_str2, N2, K2)}"

    # Test case 3
    num_str3 = "051"
    N3 = 3
    K3 = 1
    expected_result3 = 5  # 0*51 = 0 or 05*1 = 5, max is 5
    assert max_product(num_str3, N3,
                       K3) == expected_result3, f"Test case 3 failed. Expected {expected_result3}, got {max_product(num_str3, N3, K3)}"

    # If no assertion is raised, all test cases passed
    print("All test cases passed!")
test_max_product()