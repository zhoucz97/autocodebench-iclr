
class IntervalMerger {
    static List<List<Integer>> merge(List<List<Integer>> intervals) {
        """
        Merge all overlapping intervals in the given list of intervals.
        Each interval is represented as a list of two integers [start, end].
        The function should return a list of non-overlapping intervals that cover all the input intervals.
        
        Example:
        >>> merge([[1,3],[2,6],[8,10],[15,18]])
        [[1,6],[8,10],[15,18]]
        >>> merge([[1,4],[4,5]])
        [[1,5]]
        """

    if (intervals.isEmpty()) return []
    
    // Sort intervals based on start time
    intervals.sort { a, b -> a[0] <=> b[0] }
    
    def merged = []
    def currentInterval = intervals[0]
    merged.add(currentInterval)
    
    for (interval in intervals) {
        def currentEnd = currentInterval[1]
        def nextStart = interval[0]
        def nextEnd = interval[1]
        
        if (currentEnd >= nextStart) {
            // Overlapping intervals, merge them
            currentInterval[1] = Math.max(currentEnd, nextEnd)
        } else {
            // Non-overlapping interval, add to merged list
            currentInterval = interval
            merged.add(currentInterval)
        }
    }
    
    return merged
}
}
// Test cases
def intervalMerger = new IntervalMerger()
assert intervalMerger.merge([[1,3],[2,6],[8,10],[15,18]]) == [[1,6],[8,10],[15,18]]
assert intervalMerger.merge([[1,4],[4,5]]) == [[1,5]]
assert intervalMerger.merge([[1,4],[0,4]]) == [[0,4]]
assert intervalMerger.merge([[1,4],[5,6]]) == [[1,4],[5,6]]
assert intervalMerger.merge([[1,4],[2,3]]) == [[1,4]]
assert intervalMerger.merge([[1,4],[0,0]]) == [[0,0],[1,4]]
assert intervalMerger.merge([[1,4],[0,2],[3,5]]) == [[0,5]]