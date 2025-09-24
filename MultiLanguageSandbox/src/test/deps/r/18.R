

# This function 'quick_sort' implements the quick sort algorithm recursively. The algorithm sorts an array by selecting a 'pivot' element (initially the first element) and partitioning the other elements into two sub-arrays according to whether they are less than or greater than the pivot. Then it recursively applies the same strategy to the sub-arrays.
# Examples:
# quick_sort(c(3, 1, 4, 1, 5, 9, 2, 6)): This would return the sorted array c(1, 1, 2, 3, 4, 5, 6, 9).
# quick_sort(c(5, 2, 9, 3, 7)): This would return c(2, 3, 5, 7, 9).

quick_sort <- function(arr) {
  if (length(arr) <= 1) {
    return(arr)
  }
  
  pivot <- arr[1]
  less <- arr[arr < pivot]
  greater <- arr[arr > pivot]
  
  c(quick_sort(less), pivot, quick_sort(greater))
}
# Test cases
main <- function() {
    stopifnot(identical(quick_sort(c(3, 1, 4, 1, 5, 9, 2, 6)), c(1, 1, 2, 3, 4, 5, 6, 9)))
    stopifnot(identical(quick_sort(c(5, 2, 9, 3, 7)), c(2, 3, 5, 7, 9)))
    stopifnot(identical(quick_sort(c(10, 7, 8, 9, 1, 5)), c(1, 5, 7, 8, 9, 10)))
    stopifnot(identical(quick_sort(c(1, 2, 3, 4, 5)), c(1, 2, 3, 4, 5)))
    stopifnot(identical(quick_sort(c(9, 8, 7, 6, 5, 4, 3, 2, 1)), c(1, 2, 3, 4, 5, 6, 7, 8, 9)))
    stopifnot(identical(quick_sort(c()), c()))

}

# Run tests
main()