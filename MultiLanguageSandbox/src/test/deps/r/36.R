

# This function 'evaluate_expression' takes a mathematical expression as input and evaluates it to produce the result. The expression may contain parentheses, digits from 0 to 9, and operators: addition (+), subtraction (-), multiplication (*), division (/ using integer division), and exponentiation (^). The function returns the calculated value of the expression.
# Examples:
# >>> evaluate_expression("1+(3+2)*(8^2+6*9)/(2)")
#     296
# >>> evaluate_expression("2*3+4")
#     10

evaluate_expression <- function(expression) {


  expression <- gsub("\\^", "**", expression)
  tryCatch({
    expr <- parse(text = expression)
    result <- eval(expr)
    return(result)
  }, error = function(e) {
    warning("Error evaluating expression: ", e$message)
    return(NA)
  })
}
# Test cases
main <- function() {

    stopifnot(evaluate_expression("1+(3+2)*(8^2+6*9)/(2)") == 296)
    stopifnot(evaluate_expression("2*3+4") == 10)
    stopifnot(evaluate_expression("5^2-3") == 22)


}


# Run tests
main()