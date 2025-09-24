
List<int> findCorridors(List<int> counts, int limit) 
/*Finds and returns a list of indices corresponding to the highest values in the
`counts` list, up to the specified `limit`. The resulting list of indices is
sorted in ascending order.

The function first generates a list of indices (1-based) up to the specified
`limit`. Then it sorts these indices based on the corresponding values in the
`counts` list, in descending order. After sorting, it takes the indices that
correspond to the `limit` highest values and returns them sorted in ascending
order. If the `limit` is greater than the number of elements in `counts`, the
function will return indices up to the number of elements in `counts`.

Arguments:
  counts (List<int>): A list containing integer values to be ranked.
  limit (int): The maximum number of top values to return.

Returns:
  List<int>: A list containing the indices (1-based) of the top `limit` values
  from `counts`, sorted in ascending order.

Examples:
  findCorridors([10, 20, 30, 40, 50], 3) return [3, 4, 5]
*/
{
  // Handle edge case where limit is 0 or negative
  if (limit <= 0) return [];

  // Create a list of 1-based indices
  List<int> indices = List.generate(counts.length, (index) => index + 1);

  // Sort the indices based on the corresponding values in counts in descending order
  indices.sort((a, b) => counts[b - 1].compareTo(counts[a - 1]));

  // Take the top 'limit' indices, but not more than the length of counts
  int actualLimit = limit > counts.length ? counts.length : limit;
  List<int> topIndices = indices.take(actualLimit).toList();

  // Sort the top indices in ascending order
  topIndices.sort();

  return topIndices;
}
void testFindCorridors() {
  // Test case 1: General case with distinct counts
  List<int> counts1 = [10, 20, 30, 40, 50];
  int limit1 = 3;
  List<int> expected1 = [1, 2, 3]; // Indices of the top 3 counts in ascending order
  assert(findCorridors(counts1, limit1).toString() == expected1.toString());

  // Test case 2: Case with some equal counts
  List<int> counts2 = [50, 20, 50, 40, 10];
  int limit2 = 4;
  List<int> expected2 = [1, 2, 3, 4]; // Indices of the top 4 counts in ascending order
  assert(findCorridors(counts2, limit2).toString() == expected2.toString());

  // Test case 3: Case where limit exceeds the number of elements
  List<int> counts3 = [5, 15, 25];
  int limit3 = 5;
  List<int> expected3 = [1, 2, 3]; // All indices in ascending order since limit exceeds the list size
  assert(findCorridors(counts3, limit3).toString() == expected3.toString());

  // All tests passed if no assertion error is thrown
  print('All test cases passed!');
}

void main() {
  testFindCorridors();
}