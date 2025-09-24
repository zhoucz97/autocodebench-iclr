


# This function check_parentheses_balance checks if the parentheses in a given mathematical expression are balanced. A balanced expression has matching opening and closing parentheses for every pair. The function takes a single string argument, expression, which consists of lowercase English letters, operators (+, -, *, /), and parentheses. It returns "YES" if the parentheses are balanced, and "NO" otherwise. The length of the expression is less than 255 characters, and there are fewer than 20 opening parentheses.
# Examples:


# # Balanced parentheses
# check_parentheses_balance("2*(x+y)/(1-x)") # Returns "YES"

# # Unbalanced parentheses
# check_parentheses_balance("(x+y)*(2/(3*4)))") # Returns "NO"


check_parentheses_balance <- function(expression) {
  stack <- c()
  for (char in strsplit(expression, "")[[1]]) {
    if (char == "(") {
      stack <- c(stack, char)
    } else if (char == ")") {
      if (length(stack) == 0) {
        return("NO")
      }
      stack <- stack[-length(stack)]
    }
  }
  if (length(stack) == 0) {
    return("YES")
  } else {
    return("NO")
  }
}
# Test cases
main <- function() {

    stopifnot(check_parentheses_balance("2*(x+y)/(1-x)") == "YES")
    stopifnot(check_parentheses_balance("(x+y)*(2/(3*4)))") == "NO")
    stopifnot(check_parentheses_balance("((a+b)*(c-d))") == "YES")
    stopifnot(check_parentheses_balance(")(") == "NO")


}


# Run tests
main()