
import 'dart:math';
int findLargerPrimeFactor(int n)
/*Finds the larger prime factor of a given integer.

This function takes an integer `n` and returns its larger prime factor.
It starts by checking from the smallest prime number, 2, and continues
up to the square root of `n`. If a factor is found, it returns the
complementary factor, which is guaranteed to be larger. If `n` is prime
or 1, it returns `n` itself.

Args:
  int n: The integer to find the larger prime factor of.

Returns:
  The larger prime factor of `n` if it exists, otherwise `n` itself.

Examples:
  findLargerPrimeFactor(1) returns 1
  findLargerPrimeFactor(15) returns 5
*/
{
  if (n <= 1) {
    return n;
  }

  // Check for divisibility starting from 2 up to sqrt(n)
  for (int i = 2; i * i <= n; i++) {
    if (n % i == 0) {
      // If i is a factor, the complementary factor is n / i
      // Since we're checking from smallest to largest, n/i will be larger
      return n ~/ i;
    }
  }

  // If no factors found, n is prime
  return n;
}
void main() {
  testFindLargerPrimeFactor();
}

void testFindLargerPrimeFactor() {
  // Test case 1: Testing with a composite number which is a product of two primes
  assert(findLargerPrimeFactor(14) == 7, 'Test case 1 failed');

  // Test case 2: Testing with a prime number (should return the number itself)
  assert(findLargerPrimeFactor(13) == 13, 'Test case 2 failed');

  // Test case 3: Testing with a composite number with a non-prime factor
  assert(findLargerPrimeFactor(49) == 7, 'Test case 3 failed');

  // Add more cases if needed
  // ...

  print('All test cases passed!');
}