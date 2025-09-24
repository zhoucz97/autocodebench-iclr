

# Assume that the nascent resources on Earth grow at a constant rate. According to this calculation, the existing resources on the earth plus new resources can support x billion people for a year, or y billion people for b years.

# In order to achieve sustainable development and avoid resource depletion, how many billion people can the earth support?
# Examples:
# sustainable_population_limit(110, 90, 90, 210): In this case, the function returns 75.00.
sustainable_population_limit <- function(x, a, y, b){
  P <- (y * b) / (b + 1 - x / y)
  return(round(P, 2))
}
# Test cases
main <- function() {
    stopifnot(sustainable_population_limit(110, 90, 90, 210) == 75.00)
    stopifnot(sustainable_population_limit(100, 50, 50, 150) == 25.00)
    stopifnot(sustainable_population_limit(200, 100, 100, 300) == 50.00)
    stopifnot(sustainable_population_limit(120, 60, 80, 240) == 66.67)

}

# Run tests
main()