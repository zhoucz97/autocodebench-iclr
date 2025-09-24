



# This function `find_palindromic_numbers` identifies all five-digit and six-digit palindromic numbers for which the sum of the digits equals a given number 'sum_digits'.
# A palindromic number is a number that remains the same when its digits are reversed. The function takes a single argument 'sum_digits', which is a positive integer, and returns a vector of all such palindromic numbers.
# Examples:
# find_palindromic_numbers(10): This will return all five-digit and six-digit palindromic numbers whose digits sum up to 10.

find_palindromic_numbers <- function(sum_digits) {
  is_palindrome <- function(n) {
    s <- as.character(n)
    s == paste(rev(strsplit(s, "")[[1]]), collapse = "")
  }
  digit_sum <- function(n) {
    sum(as.numeric(strsplit(as.character(n), "")[[1]]))
  }
  five_digit_palindromes <- c()
  for (a in 1:9) {
    for (b in 0:9) {
      for (c in 0:9) {
        num <- a * 10000 + b * 1000 + c * 100 + b * 10 + a
        if (digit_sum(num) == sum_digits) {
          five_digit_palindromes <- c(five_digit_palindromes, num)
        }
      }
    }
  }
  six_digit_palindromes <- c()
  for (a in 1:9) {
    for (b in 0:9) {
      for (c in 0:9) {
        num <- a * 100000 + b * 10000 + c * 1000 + c * 100 + b * 10 + a
        if (digit_sum(num) == sum_digits) {
          six_digit_palindromes <- c(six_digit_palindromes, num)
        }
      }
    }
  }
  result <- sort(c(five_digit_palindromes, six_digit_palindromes))
  return(result)
}
# Test cases
main <- function() {
    stopifnot(all(find_palindromic_numbers(10) == c(10801,11611,12421,13231,14041 , 20602 , 21412  ,22222  ,23032  ,30403, 31213, 32023,  40204,  41014  ,50005 ,104401 ,113311, 122221, 131131 ,140041,203302, 212212 ,221122 ,230032, 302203, 311113 ,320023, 401104, 410014 ,500005)))
 
    stopifnot(all(find_palindromic_numbers(50) == c(799997, 889988, 898898 ,979979, 988889, 997799)) ) # Expect no palindromic numbers with digit sum 50


}

# Run tests
main()