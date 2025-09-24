
class ColorSorter {
    static void sortColors(int[] nums) {
        """
        Sort an array of integers representing colors (0 - red, 1 - white, 2 - blue)
        in place, so that the same colors are adjacent and sorted in the order
        red, white, blue.
        >>> sortColors([2, 0, 2, 1, 1, 0])
        [0, 0, 1, 1, 2, 2]
        >>> sortColors([2, 0, 1])
        [0, 1, 2]
        """

        int low = 0;      // Pointer for the end of the 0s section
        int mid = 0;      // Pointer for the current element being processed
        int high = nums.length - 1;  // Pointer for the start of the 2s section

        while (mid <= high) {
            switch (nums[mid]) {
                case 0:
                    // Swap nums[low] and nums[mid]
                    int temp = nums[low];
                    nums[low] = nums[mid];
                    nums[mid] = temp;
                    low++;
                    mid++;
                    break;
                case 1:
                    // No need to swap, just move the mid pointer
                    mid++;
                    break;
                case 2:
                    // Swap nums[mid] and nums[high]
                    temp = nums[mid];
                    nums[mid] = nums[high];
                    nums[high] = temp;
                    high--;
                    break;
            }
        }
    }

    // Helper method to print the array
    static void printArray(int[] nums) {
    }

    // Test cases
}
// Test cases
def colorSorter = new ColorSorter()
int[] nums1 = [2, 0, 2, 1, 1, 0]
colorSorter.sortColors(nums1)
assert nums1 == [0, 0, 1, 1, 2, 2]

int[] nums2 = [2, 0, 1]
colorSorter.sortColors(nums2)
assert nums2 == [0, 1, 2]

int[] nums3 = [0, 1, 2, 0, 1, 2]
colorSorter.sortColors(nums3)
assert nums3 == [0, 0, 1, 1, 2, 2]

int[] nums4 = [1, 2, 0, 1, 2, 0]
colorSorter.sortColors(nums4)
assert nums4 == [0, 0, 1, 1, 2, 2]

int[] nums5 = [2, 2, 2, 1, 1, 1, 0, 0, 0]
colorSorter.sortColors(nums5)
assert nums5 == [0, 0, 0, 1, 1, 1, 2, 2, 2]