
int findOriginalStickLength(int n, List<int> lengths)
/*
BB had some sticks of the same length, which he cut into smaller pieces such that no piece was longer than 50 units.
Now he wants to reassemble the sticks to their original lengths but has forgotten the number of original sticks and their lengths.
Given the lengths of the smaller pieces, determine the smallest possible original length of the sticks.
Args:
n (int): The number of small stick pieces.
lengths (List<int>): The lengths of the small stick pieces.
Returns:
int: The smallest possible original length of the sticks.
Example:
findOriginalStickLength(9, [5, 2, 1, 5, 2, 1, 5, 2, 1])
6
*/
{
  if (start == lengths.length) {
    return true;
  }
  if (used[start]) {
    return canFormSticks(lengths, L, start + 1, used);
  }
  for (int i = start; i < lengths.length; i++) {
    if (!used[i] && lengths[i] <= L) {
      used[i] = true;
      if (canFormSticks(lengths, L - lengths[i], start + 1, used)) {
        return true;
      }
      used[i] = false;
    }
  }
  return false;
}

int findOriginalStickLength(int n, List<int> lengths) {
  int totalSum = lengths.reduce((a, b) => a + b);
  int maxPiece = lengths.reduce((a, b) => a > b ? a : b);
  
  // Generate all possible divisors of totalSum that are >= maxPiece
  List<int> possibleL = [];
  for (int L = maxPiece; L <= totalSum; L++) {
    if (totalSum % L == 0) {
      possibleL.add(L);
    }
  }
  
  // Sort possibleL in ascending order to check the smallest first
  possibleL.sort();
  
  for (int L in possibleL) {
    List<bool> used = List.filled(n, false);
    if (canFormSticks(lengths, L, 0, used)) {
      return L;
    }
  }
  
  return -1; // should not reach here per problem statement
}
void main() {
check(findOriginalStickLength);
}

void check(int Function(int, List<int>) findOriginalStickLength) {
assert(findOriginalStickLength(9, [5, 2, 1, 5, 2, 1, 5, 2, 1]) == 6);
assert(findOriginalStickLength(4, [10, 10, 10, 10]) == 10);
assert(findOriginalStickLength(6, [15, 15, 15, 15, 30, 30]) == 30);
}