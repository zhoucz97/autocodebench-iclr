
class IntersectionFinder {
    static List<Integer> findIntersection(List<Integer> nums1, List<Integer> nums2) {
        """
        Find the intersection of two lists of integers.
        The intersection of two arrays is the list of unique elements that are present in both arrays.
        The result can be in any order.
        >>> findIntersection([1, 2, 2, 1], [2, 2])
        [2]
        >>> findIntersection([4, 9, 5], [9, 4, 9, 8, 4])
        [9, 4]
        """

    // Convert both lists to sets to remove duplicates and then find the intersection
    def set1 = nums1.toSet()
    def set2 = nums2.toSet()
    
    // Find the intersection of the two sets
    def intersection = set1.intersect(set2)
    
    // Convert the result back to a list (order doesn't matter as per requirements)
    return intersection.toList()
}
}
// Test cases
def intersectionFinder = new IntersectionFinder()
assert intersectionFinder.findIntersection([1, 2, 2, 1], [2, 2]) == [2]
assert intersectionFinder.findIntersection([4, 9, 5], [9, 4, 9, 8, 4]) == [4, 9]
assert intersectionFinder.findIntersection([1, 2, 3, 4, 5], [6, 7, 8, 9, 10]) == []
assert intersectionFinder.findIntersection([1, 2, 3, 4, 5], [5, 4, 3, 2, 1]) == [1, 2, 3, 4, 5]
assert intersectionFinder.findIntersection([1, 1, 2, 2, 3, 3], [2, 2, 3, 3, 4, 4]) == [2, 3]
assert intersectionFinder.findIntersection([1, 2, 3, 4, 5], [1, 2, 3, 4, 5]) == [1, 2, 3, 4, 5]