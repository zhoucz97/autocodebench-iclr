class QuickSortProgram {

    static List<Integer> quicksort(List<Integer> list) {
        if (list == null || list.size() <= 1) {
            return list ?: []
        }

        int pivot = list[0]
        List<Integer> rest = list[1..-1]
        List<Integer> less = rest.findAll { it < pivot }
        List<Integer> greater = rest.findAll { it >= pivot }

        return quicksort(less) + [pivot] + quicksort(greater)
    }

    static void checkQuicksort() {
        assert quicksort([3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]) == [1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 9]
        assert quicksort([5, 4, 3, 2, 1]) == [1, 2, 3, 4, 5]
        assert quicksort([]) == []
        assert quicksort([1]) == [1]
    }

    static void main(String[] args) {
        checkQuicksort()
    }
}
