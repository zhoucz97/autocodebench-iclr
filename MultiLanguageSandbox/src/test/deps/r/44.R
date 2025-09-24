

# This function compare_area determines who has the larger area between Alice's square and Bob's rectangle. 
# Alice has a square with side length 'a', and Bob has a rectangle with length 'b' and width 'c'. 
# The function returns "Alice" if the area of the square is greater than that of the rectangle, 
# and "Bob" if the rectangle's area is greater. If the areas are equal, it returns "Equal".
# Examples:
# >>> compare_area(5, 4, 6)
#     "Alice"
# >>> compare_area(7, 5, 10)
#     "Bob"
compare_area <- function(a, b, c) {
  alice_area <- a^2
  bob_area <- b * c
  
  if (alice_area > bob_area) {
    return("Alice")
  } else if (bob_area > alice_area) {
    return("Bob")
  } else {
    return("Equal")
  }
}
# Test cases
main <- function() {
    stopifnot(compare_area(5, 4, 6) == "Alice")
    stopifnot(compare_area(7, 5, 10) == "Bob")
    stopifnot(compare_area(5, 5, 5) == "Equal")


}


# Run tests
main()