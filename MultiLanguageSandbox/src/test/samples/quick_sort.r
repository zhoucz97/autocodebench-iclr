quicksort <- function(lst) {
  if (is.null(lst) || length(lst) <= 1) {
    if (is.null(lst)) {
      return(numeric(0))
    }
    return(lst)
  }
  
  pivot <- lst[1]
  rest <- lst[-1]
  less <- rest[rest < pivot]
  greater <- rest[rest >= pivot]
  
  return(c(quicksort(less), pivot, quicksort(greater)))
}

check_quicksort <- function() {
  stopifnot(identical(quicksort(c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)), c(1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 9)))
  stopifnot(identical(quicksort(c(5, 4, 3, 2, 1)), c(1, 2, 3, 4, 5)))
  stopifnot(identical(quicksort(numeric(0)), numeric(0)))
  stopifnot(identical(quicksort(c(1)), c(1)))
}

check_quicksort()
