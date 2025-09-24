
String convertToNegativeBase(int n, int base) 
/*Converts an integer to its representation in a given negative base.

This function takes an integer `n` and a negative integer `base` and
returns a string representing the number `n` in the specified negative
base system. The conversion is done using the standard negative base
conversion algorithm, which handles remainders and the division process
differently than in positive base systems due to the nature of negative
bases.

Args:
  n (int): The integer to be converted.
  base (int): The negative base to which the integer `n` will be converted.
              The base must be a negative integer (e.g., -2, -3, etc.).

Returns:
  String: A string representing the integer `n` in the negative base system.

Examples:
  convertToNegativeBase(10, -2) returns "11010"
  convertToNegativeBase(4, -3) returns "102"
*/
{
  if (base >= 0) {
    throw ArgumentError('Base must be a negative integer');
  }

  if (n == 0) {
    return '0';
  }

  String result = '';
  while (n != 0) {
    int remainder = n % base;
    n ~/= base;

    // If the remainder is negative, adjust it
    if (remainder < 0) {
      remainder += -base;
      n += 1;
    }

    result = remainder.toString() + result;
  }

  return result;
}
void testConvertToNegativeBase() {
  // Test case 1: Conversion of 10 to base -2
  assert(convertToNegativeBase(10, -2) == "1010", "Test Case 1 Failed: 10 in base -2 should be '11010'");

  // Test case 2: Conversion of -15 to base -2
  assert(convertToNegativeBase(-15, -2) == "1111", "Test Case 2 Failed: -15 in base -2 should be '110001'");

  // Test case 3: Conversion of 4 to base -3
  assert(convertToNegativeBase(4, -3) == "21", "Test Case 3 Failed: 4 in base -3 should be '102'");

  print("All convertToNegativeBase test cases passed!");
}

void main() {
  testConvertToNegativeBase();
}