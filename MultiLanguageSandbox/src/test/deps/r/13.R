


# This function 'calculate_total_amount' calculates the total amount after one year for a given principal amount and an annual interest rate. The function takes into account a 5% tax on the interest earned.
# The principal amount is in RMB and the interest rate is given as a percentage (without the percent sign).
# The result is rounded to the nearest cent.
# Examples:
# calculate_total_amount(5000.00, 5.00): This should return 5237.50, representing the total amount after one year including taxed interest.
# calculate_total_amount(10000.00, 3.50): This should return 10347.25.


calculate_total_amount <- function(principal, interest_rate) {
  interest_earned <- principal * (interest_rate / 100)
  tax_on_interest <- interest_earned * 0.05
  interest_after_tax <- interest_earned - tax_on_interest
  total_amount <- principal + interest_after_tax
  total_amount_rounded <- round(total_amount, 2)
  
  return(total_amount_rounded)
}
# Test cases
main <- function() {

    stopifnot(calculate_total_amount(5000.00, 5.00) == 5237.50)
    stopifnot(calculate_total_amount(10000.00, 3.50) == 10332.50)
    stopifnot(calculate_total_amount(15000.00, 4.00) == 15570.00)
    stopifnot(calculate_total_amount(20000.00, 6.00) == 21140.00)
    stopifnot(calculate_total_amount(30000.00, 1.00) == 30285.00)

}

# Run tests
main()