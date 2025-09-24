
String getCantorNumber(int n)
/*Finds the N-th term in the Cantor enumeration of the rational numbers.

The Cantor enumeration is a sequence that lists all positive rational
numbers (fractions) uniquely. The enumeration is based on traversing the
infinite matrix of numerators/denominators diagonally. This function
calculates the numerator and denominator of the N-th term in this sequence.

Args:
  n (int): The position in the sequence for which to calculate the
           numerator and denominator.

Returns:
  String: A string representation of the fraction at the N-th position
          in the form "numerator/denominator".

Examples:
  getCantorNumber(1) returns "1/1"
  getCantorNumber(6) returns "1/3"
*/
{
  if (n < 1) return "0/0"; // handle invalid input, though problem says n is positive
  
  int k = 1;
  while (k * (k + 1) / 2 < n) {
    k++;
  }
  
  int posInDiagonal = n - (k * (k - 1) / 2);
  
  int numerator, denominator;
  if (k % 2 == 1) {
    // Odd diagonal: numerator increases, denominator decreases
    numerator = posInDiagonal;
    denominator = k + 1 - posInDiagonal;
  } else {
    // Even diagonal: numerator decreases, denominator increases
    numerator = k + 1 - posInDiagonal;
    denominator = posInDiagonal;
  }
  
  return "$numerator/$denominator";
}
void testGetCantorNumber() {
  // Test case 1: First term (N=1)
  assert(getCantorNumber(1) == "1/1", "Test Case 1 Failed: N=1 should result in '1/1'");

  // Test case 2: Fifth term (N=5)
  assert(getCantorNumber(5) == "2/2", "Test Case 2 Failed: N=5 should result in '2/2'");

  // Test case 3: Tenth term (N=10)
  assert(getCantorNumber(10) == "4/1", "Test Case 3 Failed: N=10 should result in '1/4'");

  print("All getCantorNumber test cases passed!");
}

void main() {
  testGetCantorNumber();
}