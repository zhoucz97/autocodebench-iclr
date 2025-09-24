
List<int> removeDuplicatesAndSort(List<int> numbers)
/*Removes duplicates from a list of integers and returns a sorted list of the unique numbers.

This function first converts the input list to a set to remove any duplicate values.
Since a set inherently contains only unique elements, this step ensures that all duplicates
are eliminated. The set is then converted back to a list, and this list is sorted in ascending
order before being returned.

Arguments:
  numbers (List<int>): The list of integers to be processed, which may contain duplicates.

Returns:
  List<int>: A list of unique integers sorted in ascending order.

Examples:
    removeDuplicatesAndSort([4, 1, 2, 4, 3, 2])
    return [1, 2, 3, 4].
    removeDuplicatesAndSort([1, 2, 2, 3])
    return [1, 2, 3].
*/
{
  // Convert the list to a set to remove duplicates
  Set<int> uniqueNumbers = Set.from(numbers);
  
  // Convert the set back to a list and sort it in ascending order
  List<int> sortedUniqueNumbers = uniqueNumbers.toList()..sort();
  
  return sortedUniqueNumbers;
}
void testRemoveDuplicatesAndSort() {
  // Test case 1: A list with duplicates that needs sorting
  assert(
    removeDuplicatesAndSort([4, 1, 2, 4, 3, 2]).toString() == [1, 2, 3, 4].toString(),
    'Test Case 1 Failed: Expected [1, 2, 3, 4], got ${removeDuplicatesAndSort([4, 1, 2, 4, 3, 2])}'
  );

  // Test case 2: A list with no duplicates, only needs sorting
  assert(
    removeDuplicatesAndSort([5, 3, 4, 1, 2]).toString() == [1, 2, 3, 4, 5].toString(),
    'Test Case 2 Failed: Expected [1, 2, 3, 4, 5], got ${removeDuplicatesAndSort([5, 3, 4, 1, 2])}'
  );

  // Test case 3: A list that is already sorted and has duplicates
  assert(
    removeDuplicatesAndSort([1, 2, 2, 3]).toString() == [1, 2, 3].toString(),
    'Test Case 3 Failed: Expected [1, 2, 3], got ${removeDuplicatesAndSort([1, 2, 2, 3])}'
  );

  // Additional test cases can be added here as needed.

  print('All test cases passed!');
}

void main() {
  testRemoveDuplicatesAndSort();
}