
List<List<int>> generatePermutations(List<int> list)
/*Generates all possible permutations of a given list of integers.

This recursive function takes a list of integers as input and returns
a list of lists, where each inner list represents a unique permutation
of the input list. The function works by iterating over each element in
the list, removing it, and generating all permutations of the remaining
elements. Each of these permutations has the removed element prepended
to it, thereby creating a new permutation that includes the removed element.
The base case of the recursion is when the list contains only one element,
in which case, the list itself is the only permutation.

Args:
  list (List<int>): The list of integers to permute.

Returns:
  List<List<int>>: A list containing all permutations of the input list.

Examples:
  generatePermutations([1]) returns [[1]]
  generatePermutations([1, 2]) returns [[1, 2], [2, 1]]
*/
{
  // Base case: if the list has only one element, return it as the only permutation
  if (list.length == 1) {
    return [List.from(list)];
  }

  List<List<int>> permutations = [];

  for (int i = 0; i < list.length; i++) {
    // Remove the current element
    int currentElement = list[i];
    List<int> remainingElements = List.from(list)..removeAt(i);

    // Recursively generate permutations of the remaining elements
    List<List<int>> subPermutations = generatePermutations(remainingElements);

    // Prepend the current element to each sub-permutation
    for (List<int> subPermutation in subPermutations) {
      subPermutation.insert(0, currentElement);
      permutations.add(subPermutation);
    }
  }

  return permutations;
}
void testGeneratePermutations() {
  // Test case 1
  List<int> list1 = [1];
  List<List<int>> expectedPerms1 = [
    [1]
  ];
  assert(ListEquality().equals(generatePermutations(list1), expectedPerms1));

  // Test case 2
  List<int> list2 = [1, 2];
  List<List<int>> expectedPerms2 = [
    [1, 2],
    [2, 1]
  ];
  assert(ListEquality().equals(generatePermutations(list2), expectedPerms2));

  // Test case 3
  List<int> list3 = [1, 2, 3];
  List<List<int>> expectedPerms3 = [
    [1, 2, 3],
    [1, 3, 2],
    [2, 1, 3],
    [2, 3, 1],
    [3, 1, 2],
    [3, 2, 1]
  ];
  assert(ListEquality().equals(generatePermutations(list3), expectedPerms3));

  print("All test cases passed!");
}

// Equality for nested lists
class ListEquality {
  bool equals(List<List<int>> list1, List<List<int>> list2) {
    if (list1.length != list2.length) return false;
    for (int i = 0; i < list1.length; i++) {
      if (!_listEquals(list1[i], list2[i])) {
        return false;
      }
    }
    return true;
  }

  bool _listEquals(List<int> list1, List<int> list2) {
    if (list1.length != list2.length) return false;
    for (int i = 0; i < list1.length; i++) {
      if (list1[i] != list2[i]) {
        return false;
      }
    }
    return true;
  }
}

void main() {
  testGeneratePermutations();
}