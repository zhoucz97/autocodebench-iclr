
String concatenateLargestNumber(List<int> numbers)
/*Creates the largest possible number by concatenating the given list of integers.

The function sorts the list of integers in such a way that when they are
concatenated in this order, the resulting string represents the largest
possible number. It uses a custom comparator for sorting that compares
concatenated pairs of numbers in reverse order to ensure the correct
placement for the largest concatenated result.

Args:
  numbers (List<int>): A list of non-negative integers to be concatenated.

Returns:
  String: A string representing the largest number formed by the
          concatenation of the provided integers.

Examples:
  concatenateLargestNumber([10, 2]) returns "210"
  concatenateLargestNumber([42]) returns "42"
*/
{
  if (numbers.isEmpty) return "0";
  
  // Convert all numbers to strings for easy concatenation
  List<String> strNumbers = numbers.map((num) => num.toString()).toList();
  
  // Custom comparator to decide the order of concatenation
  strNumbers.sort((a, b) {
    String order1 = a + b;
    String order2 = b + a;
    return order2.compareTo(order1); // Descending order
  });
  
  // Handle the case where the largest number is 0 (e.g., [0, 0])
  if (strNumbers[0] == "0") {
    return "0";
  }
  
  // Concatenate all strings in the sorted list
  return strNumbers.join();
}
void testConcatenateLargestNumber() {
  // Test case 1: Simple case
  assert(concatenateLargestNumber([10, 2]) == "210", "Test Case 1 Failed: [10, 2] should concatenate to '210'");

  // Test case 2: Case with same starting digits
  assert(concatenateLargestNumber([3, 30, 34, 5, 9]) == "9534330", "Test Case 2 Failed: [3, 30, 34, 5, 9] should concatenate to '9534330'");

  // Test case 3: Case with zeros
  assert(concatenateLargestNumber([0, 0, 0, 1]) == "1000", "Test Case 3 Failed: [0, 0, 0, 1] should concatenate to '1000'");

  // Test case 4: All zeros
  assert(concatenateLargestNumber([0, 0, 0, 0]) == "0000", "Test Case 4 Failed: [0, 0, 0, 0] should concatenate to '0'");

  // Test case 5: Single number
  assert(concatenateLargestNumber([42]) == "42", "Test Case 5 Failed: [42] should concatenate to '42'");

  print("All concatenateLargestNumber test cases passed!");
}

void main() {
  testConcatenateLargestNumber();
}