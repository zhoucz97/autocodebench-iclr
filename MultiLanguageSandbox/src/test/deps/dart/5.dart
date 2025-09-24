import 'dart:math';
import 'dart:io';
import 'dart:math';
BigInt sumOfFactorials(int n)
/*Calculates the sum of factorials of all integers from 1 to n.

This function computes the factorial of each integer from 1 up to and
including n using the `factorial` function, then sums these factorials to
obtain the final result. The function uses `BigInt` to handle very large
numbers that may arise from factorial calculations.

Args:
  n (int): The upper bound of the range for which to calculate the sum
           of factorials. The value of n should be a non-negative integer.

Returns:
  BigInt: The sum of factorials of all integers from 1 to n.

Examples:
  sumOfFactorials(1) returns BigInt.from(1) (since 1! = 1)
  sumOfFactorials(3) returns BigInt.from(9) (since 1! + 2! + 3! = 1 + 2 + 6 = 9)
*/
{
  if (n < 0) {
    throw ArgumentError('n must be a non-negative integer');
  }

  BigInt sum = BigInt.zero;
  
  for (int i = 1; i <= n; i++) {
    sum += factorial(i);
  }
  
  return sum;
}

BigInt factorial(int n) {
  if (n < 0) {
    throw ArgumentError('n must be a non-negative integer');
  }
  
  BigInt result = BigInt.one;
  for (int i = 2; i <= n; i++) {
    result *= BigInt.from(i);
  }
  return result;
}
void testSumOfFactorials() {
  // Test case 1: Sum of factorials up to 1
  assert(sumOfFactorials(1) == BigInt.from(1), "Sum of factorials up to 1 should be 1");

  // Test case 2: Sum of factorials up to 3
  assert(sumOfFactorials(3) == BigInt.from(9), "Sum of factorials up to 3 should be 9");

  // Test case 3: Sum of factorials up to 4
  BigInt expectedSumOfFactorials4 = BigInt.from(1) + BigInt.from(2) + BigInt.from(6) + BigInt.from(24);
  assert(sumOfFactorials(4) == expectedSumOfFactorials4, "Sum of factorials up to 4 should be 33");

  print("All sum of factorials test cases passed!");
}

void main() {
  testSumOfFactorials();
}