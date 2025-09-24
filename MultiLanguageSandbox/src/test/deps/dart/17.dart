
String formatPolynomial(List<int> coefficients)
/*Formats a list of coefficients into a string representation of a polynomial.

This function takes a list of integer coefficients and returns a string that represents
the corresponding polynomial. Coefficients are listed in descending order of their degree.
Zero coefficients are skipped, and the powers of 'x' are included only where necessary.
The output polynomial does not contain unnecessary plus or minus signs and avoids using
the coefficient '1' where it is implied, except for the constant term.

Arguments:
  coefficients (List<int>): A list of integer coefficients for the polynomial.

Returns:
  String: A string representation of the polynomial.

Examples:
  formatPolynomial([1, 0, -4, 3])
  return 'x^3 - 4x + 3'
*/
{
  if (coefficients.isEmpty) return "0";
  
  List<String> terms = [];
  
  for (int i = 0; i < coefficients.length; i++) {
    int coeff = coefficients[i];
    int degree = coefficients.length - 1 - i;
    
    if (coeff == 0) continue;
    
    String term;
    
    if (degree == 0) {
      // Constant term
      term = coeff.toString();
    } else if (degree == 1) {
      // Linear term (x)
      if (coeff == 1) {
        term = "x";
      } else if (coeff == -1) {
        term = "-x";
      } else {
        term = "$coeff x";
      }
    } else {
      // Higher degree terms (x^n)
      if (coeff == 1) {
        term = "x^$degree";
      } else if (coeff == -1) {
        term = "-x^$degree";
      } else {
        term = "$coeff x^$degree";
      }
    }
    
    terms.add(term);
  }
  
  if (terms.isEmpty) return "0";
  
  // Handle signs
  StringBuffer result = StringBuffer();
  bool firstTerm = true;
  
  for (String term in terms) {
    if (firstTerm) {
      result.write(term);
      firstTerm = false;
    } else {
      if (term.startsWith('-')) {
        result.write(" $term");
      } else {
        result.write(" + $term");
      }
    }
  }
  
  return result.toString();
}
void testFormatPolynomial() {
  // Test case 1: Polynomial with positive and negative coefficients, and zero coefficients
  assert(
    formatPolynomial([3, -2, 0, 1]) == "3x^3-2x^2+1",
    'Test Case 1 Failed: Expected polynomial string to be "3x^3-2x^2+x", got "${formatPolynomial([3, -2, 0, 1])}"'
  );

  // Test case 2: Polynomial with all positive coefficients
  assert(
    formatPolynomial([1, 2, 3]) == "x^2+2x+3",
    'Test Case 2 Failed: Expected polynomial string to be "x^2+2x+3", got "${formatPolynomial([1, 2, 3])}"'
  );

  // Test case 3: Polynomial with a single constant term
  assert(
    formatPolynomial([5]) == "5",
    'Test Case 3 Failed: Expected polynomial string to be "5", got "${formatPolynomial([5])}"'
  );

  // Additional test cases can be added here as needed.

  print('All test cases passed!');
}

void main() {
  testFormatPolynomial();
}