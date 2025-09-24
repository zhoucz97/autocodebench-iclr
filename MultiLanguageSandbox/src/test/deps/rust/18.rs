
/*
  Calculates the value of the polynomial f(x) = ax^3 + bx^2 + cx + d for a given set of coefficients and the value of x.
  This function uses real numbers for both the coefficients and the value of x, providing the polynomial's value as a double-precision floating-point number.

  Arguments:
  - x: The value at which the polynomial is evaluated.
  - a: The coefficient of x^3.
  - b: The coefficient of x^2.
  - c: The coefficient of x.
  - d: The constant term in the polynomial.

  Returns:
  - The computed value of the polynomial at x, as a double-precision floating-point number.

  Example:
  - polynomial_value(2.31, 1.2, 2.0, 2.0, 3.0) returns 33.0838692
*/



fn polynomial_value(x: f64, a: f64, b: f64, c: f64, d: f64) -> f64 {
    a * x.powi(3) + b * x.powi(2) + c * x + d
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert!((polynomial_value(2.31, 1.2, 2.0, 2.0, 3.0) - 33.0838692).abs() < 1e-7);
        assert!((polynomial_value(0.0, 1.0, 1.0, 1.0, 1.0) - 1.0).abs() < 1e-7);
        assert!((polynomial_value(1.0, 1.0, 1.0, 1.0, 1.0) - 4.0).abs() < 1e-7);
    }
    

}