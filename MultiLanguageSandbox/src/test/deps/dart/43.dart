
String removeKDigits(String num, int k)

/*
Through keyboard input, a high-precision positive integer $N$ (no more than $250$ digits) is entered. By removing any $k$ digits from it, the remaining digits will form a new non-negative integer, preserving the original left-to-right order. The task is to develop a program, given $N$ and $k$, that finds a strategy to ensure the newly formed number is the smallest possible.

## Input Format

num: a string representing the high-precision positive integer $N$.
k: an integer representing the number of digits to be removed.

## Output Format

Output one integer, the smallest possible number remaining after deletion.

## Sample #1
removeKDigits("175438", 4) => 13
*/
{
  List<String> stack = [];
  for (int i = 0; i < num.length; i++) {
    String current = num[i];
    while (stack.isNotEmpty && k > 0 && stack.last.compareTo(current) > 0) {
      stack.removeLast();
      k--;
    }
    stack.add(current);
  }
  
  // If there are still digits to remove, remove them from the end
  while (k > 0) {
    stack.removeLast();
    k--;
  }
  
  // Construct the result string and remove leading zeros
  String result = stack.join('');
  result = result.replaceAll(RegExp('^0+'), '');
  
  return result.isEmpty ? '0' : result;
}
void main() {
  testRemoveKDigits();
}

void testRemoveKDigits() {
  assert(removeKDigits("175438", 4) == "13");
  assert(removeKDigits("10200", 1) == "200");
  assert(removeKDigits("1432219", 3) == "1219");
}