

from typing import List
def count_apples_reachable(apple_heights: List[int], tao_reach: int) -> int:
    """
    Calculate the number of apples that Tao Tao can reach.

    Args:
    apple_heights (List[int]): The heights of apples from the ground (in centimeters).
    tao_reach (int): The maximum height Tao Tao can reach with her hand stretched upwards (in centimeters).

    Returns:
    int: The number of apples Tao Tao can reach with or without standing on a 30cm stool.

    This function iterates over the list of apple heights and counts how many of them are within Tao Tao's reach,
    considering an additional 30cm that the stool provides.

    Example cases:
       count_apples_reachable([100, 120, 130, 140, 150, 150, 140, 130, 120, 110], 120) -> returns 10
       count_apples_reachable([200, 190, 180, 170, 160, 150, 140, 135, 130, 131], 100) -> returns 0
    """
    """
    Calculate the number of apples that Tao Tao can reach.

    Args:
    apple_heights (List[int]): The heights of apples from the ground (in centimeters).
    tao_reach (int): The maximum height Tao Tao can reach with her hand stretched upwards (in centimeters).

    Returns:
    int: The number of apples Tao Tao can reach with or without standing on a 30cm stool.
    """
    count = 0
    for height in apple_heights:
        if height <= tao_reach + 30:
            count += 1
    return count
import unittest

class TestTaoTaoApplePicking(unittest.TestCase):
    def test_count_apples_reachable(self):
        # Test case 1: Tao Tao can reach all apples
        apples_1 = [100, 105, 110, 115, 120, 125, 130, 135, 140, 145]
        tao_reach_1 = 120
        self.assertEqual(count_apples_reachable(apples_1, tao_reach_1), 10)

        # Test case 2: Tao Tao can't reach any apples
        apples_2 = [200, 200, 200, 200, 200, 200, 200, 200, 200, 200]
        tao_reach_2 = 100
        self.assertEqual(count_apples_reachable(apples_2, tao_reach_2), 0)

        # Test case 3: Tao Tao can reach some of the apples
        apples_3 = [160, 150, 140, 170, 180, 190, 160, 150, 145, 155]
        tao_reach_3 = 115
        self.assertEqual(count_apples_reachable(apples_3, tao_reach_3), 2)

# To run the tests
if __name__ == '__main__':
    unittest.main()