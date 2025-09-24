import java.util.PriorityQueue
"""
class MedianFinder {

    public MedianFinder() {

    }
    
    public void addNum(int num) {

    }
    
    public double findMedian() {

    }
}
This class is used to find the median of a data stream.
The data stream is a list of integers.
The median is the middle value in an ordered integer list.
If the size of the list is even, there is no single middle value;
the median is then usually defined to be the mean of the two middle values.

For example:
If we add the numbers 2, 3, 4 in order, the median is 3.
If we add the numbers 2, 3, 4, 5 in order, the median is (3 + 4) / 2 = 3.5.
"""

        maxHeap = new PriorityQueue<>(Collections.reverseOrder())
        minHeap = new PriorityQueue<>()
    }
    
    public void addNum(int num) {
        if (maxHeap.isEmpty() || num <= maxHeap.peek()) {
            maxHeap.offer(num)
        } else {
            minHeap.offer(num)
        }
        
        // Balance the heaps
        if (maxHeap.size() > minHeap.size() + 1) {
            minHeap.offer(maxHeap.poll())
        } else if (minHeap.size() > maxHeap.size()) {
            maxHeap.offer(minHeap.poll())
        }
    }
    
    public double findMedian() {
        if (maxHeap.size() == minHeap.size()) {
            return (maxHeap.peek() + minHeap.peek()) / 2.0
        } else {
            return maxHeap.peek()
        }
    }
def medianFinder = new MedianFinder()
medianFinder.addNum(1)
medianFinder.addNum(2)
assert medianFinder.findMedian() == 1.5
medianFinder.addNum(3)
assert medianFinder.findMedian() == 2.0

medianFinder = new MedianFinder()
medianFinder.addNum(1)
medianFinder.addNum(2)
medianFinder.addNum(3)
medianFinder.addNum(4)
assert medianFinder.findMedian() == 2.5
medianFinder.addNum(5)
assert medianFinder.findMedian() == 3.0