

def optimize_seating(M: int, N: int, K: int, L: int, D: int, chatting_pairs: list) -> (str, str):
    """
    Optimize the placement of aisles in a classroom to minimize the amount of chatting between students.

    Args:
    M (int): The number of rows in the classroom.
    N (int): The number of columns in the classroom.
    K (int): The number of horizontal aisles to add.
    L (int): The number of vertical aisles to add.
    D (int): The number of chatting pairs in the classroom.
    chatting_pairs (list of tuples): A list of tuples, each containing the positions (Xi, Yi) and (Pi, Qi) of a chatting pair.

    Returns:
    (str, str): Two space-separated strings representing the optimal row and column indices for the aisles.

    The function works by counting the number of chatting pairs that can be separated by adding an aisle in each possible position.
    It then selects the most effective positions for aisles, aiming to separate as many chatting pairs as possible.

    Examples:
    - optimize_seating(4, 5, 1, 2, 3, [(4, 2, 4, 3), (2, 3, 3, 3), (2, 5, 2, 4)])
    Returns: ('2', '2 4')

    - optimize_seating(3, 3, 1, 1, 2, [(1, 2, 1, 3), (2, 1, 3, 1)])
    Returns: ('2', '2')
    """
    # Initialize dictionaries to count separations for each possible aisle position
    horizontal_counts = {}
    vertical_counts = {}
    
    # Process each chatting pair to count separations for all possible aisle positions
    for pair in chatting_pairs:
        x1, y1, x2, y2 = pair
        # Check for horizontal aisles (between rows)
        if x1 != x2:
            min_row = min(x1, x2)
            max_row = max(x1, x2)
            # The aisle must be placed between min_row and max_row (i.e., after min_row)
            for aisle_row in range(min_row, max_row):
                if aisle_row not in horizontal_counts:
                    horizontal_counts[aisle_row] = 0
                horizontal_counts[aisle_row] += 1
        # Check for vertical aisles (between columns)
        if y1 != y2:
            min_col = min(y1, y2)
            max_col = max(y1, y2)
            # The aisle must be placed between min_col and max_col (i.e., after min_col)
            for aisle_col in range(min_col, max_col):
                if aisle_col not in vertical_counts:
                    vertical_counts[aisle_col] = 0
                vertical_counts[aisle_col] += 1
    
    # Select top K horizontal aisles (if K > 0)
    sorted_horizontal = sorted(horizontal_counts.items(), key=lambda item: (-item[1], item[0]))
    selected_horizontal = [str(pos) for pos, count in sorted_horizontal[:K]]
    
    # Select top L vertical aisles (if L > 0)
    sorted_vertical = sorted(vertical_counts.items(), key=lambda item: (-item[1], item[0]))
    selected_vertical = [str(pos) for pos, count in sorted_vertical[:L]]
    
    # Prepare the output strings
    horizontal_str = ' '.join(selected_horizontal) if selected_horizontal else ''
    vertical_str = ' '.join(selected_vertical) if selected_vertical else ''
    
    return (horizontal_str, vertical_str)
def test_optimize_seating():
    test_cases = [
        # Test case 1 from the provided example
        {
            'M': 4, 'N': 5, 'K': 1, 'L': 2, 'D': 3,
            'chatting_pairs': [(4, 2, 4, 3), (2, 3, 3, 3), (2, 5, 2, 4)],
            'expected': ('2', '2 4')
        },
        # Corrected Test case 2
        {
            'M': 5, 'N': 6, 'K': 2, 'L': 3, 'D': 4,
            'chatting_pairs': [(1, 1, 1, 2), (2, 2, 3, 2), (4, 5, 4, 6), (5, 1, 5, 2)],
            'expected': ('1 2', '1 2 5')  # Corrected expected output
        },
        # Test case 3
        {
            'M': 3, 'N': 3, 'K': 1, 'L': 1, 'D': 2,
            'chatting_pairs': [(1, 2, 1, 3), (2, 1, 3, 1)],
            'expected': ('2', '2')
        },
    ]

    for i, test in enumerate(test_cases, 1):
        row_output, col_output = optimize_seating(
            test['M'], test['N'], test['K'], test['L'], test['D'], test['chatting_pairs']
        )
        assert row_output == test['expected'][0], f"Test case {i} failed: row_output {row_output} does not match expected {test['expected'][0]}"
        assert col_output == test['expected'][1], f"Test case {i} failed: col_output {col_output} does not match expected {test['expected'][1]}"
        print(f"Test case {i} passed.")

# Run the test function
test_optimize_seating()