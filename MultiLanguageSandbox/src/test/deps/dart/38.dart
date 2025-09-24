
String expandString(String input, int p1, int p2, int p3)
/*
We provided an example of string expansion: if the input string contains substrings such as `d-h` or `4-8`, we treat it as a kind of abbreviation. During output, continuous increasing letters or number strings replace the dash, i.e., the above two substrings are output respectively as `defgh` and `45678`. In this question, we make the string expansion more flexible by adding some parameter settings. The specific agreements are as follows:

(1) The need for string expansion arises in the following scenario: in the input string, a dash `-` appears, with both sides of the dash being either lowercase letters or numbers, and according to the ASCII code order, the character to the right of the dash is strictly greater than the character on the left.

(2) Parameter $p_1$: Expansion mode. When $p_1=1$, for letter substrings, fill with lowercase letters; when $p_1=2$, for letter substrings, fill with uppercase letters. In these two cases, the filling method for number substrings is the same. When $p_1=3$, regardless of whether it is a letter substring or a number substring, it is filled with asterisks `*` equal in number to the letters that need to be filled.

(3) Parameter $p_2$: The repetition count of the fill character. $p_2=k$ means that the same character should be filled continuously $k$ times. For example, when $p_2=3$, the substring `d-h` should be expanded to `deeefffgggh`. The characters on both sides of the dash remain unchanged.

(4) Parameter $p_3$: Whether to reverse the order: $p_3=1$ means to maintain the original order, $p_3=2$ means to use reverse order output, noting that the characters on both ends of the dash are still not included. For example, when $p_1=1$, $p_2=2$, $p_3=2$, the substring `d-h` should be expanded to `dggffeeh`.

(5) If the character to the right of the dash is exactly the successor of the character on the left, only the dash in the middle is removed, for example: `d-e` should be output as `de`, `3-4` should be output as `34`. If the character to the right of the dash is less than or equal to the character on the left according to the ASCII code sequence, the dash in the middle should be retained during output, for example: `d-d` should be output as `d-d`, `3-1` should be output as `3-1`.

## Input Format

There are two lines in total.

The 1st line contains 3 positive integers separated by spaces, representing parameters $p_1,p_2,p_3$ in sequence.

The 2nd line is a string composed only of numbers, lowercase letters, and the dash `-`. There is no space at the beginning or at the end of the line.

## Output Format

There is a single line for output, which is the string after expansion.

## Sample #1
expandString("abcs-w1234-9s-4zz", 1, 2, 1) => "abcsttuuvvw1234556677889s-4zz"
*/
{
  StringBuffer result = StringBuffer();
  int i = 0;
  while (i < input.length) {
    if (i + 2 < input.length && input[i + 1] == '-' && 
        ((input[i].toLowerCase() != input[i] || input[i + 2].toLowerCase() != input[i + 2]) // letters
         || (input[i].codeUnitAt(0) >= '0'.codeUnitAt(0) && input[i].codeUnitAt(0) <= '9'.codeUnitAt(0) 
             && input[i + 2].codeUnitAt(0) >= '0'.codeUnitAt(0) && input[i + 2].codeUnitAt(0) <= '9'.codeUnitAt(0)))) { // numbers
      String leftChar = input[i];
      String rightChar = input[i + 2];
      int leftCode = leftChar.codeUnitAt(0);
      int rightCode = rightChar.codeUnitAt(0);
      
      if (rightCode > leftCode) {
        // Valid expansion candidate
        if (rightCode == leftCode + 1) {
          // Immediate successor, just remove the dash
          result.write(leftChar);
          result.write(rightChar);
          i += 3;
        } else {
          // Need to expand
          List<String>中间Chars = [];
          for (int code = leftCode + 1; code < rightCode; code++) {
            String c;
            if (p1 == 1) {
              c = String.fromCharCode(code).toLowerCase();
            } else if (p1 == 2) {
              c = String.fromCharCode(code).toUpperCase();
            } else { // p1 == 3
              c = '*';
            }
            中间Chars.add(c * p2);
          }
          
          if (p3 == 2) {
            中间Chars = 中间Chars.reversed.toList();
          }
          
          String expandedMiddle = 中间Chars.join();
          result.write(leftChar);
          result.write(expandedMiddle);
          result.write(rightChar);
          i += 3;
        }
      } else {
        // Right not greater, keep as is
        result.write(input[i]);
        i++;
      }
    } else {
      result.write(input[i]);
      i++;
    }
  }
  return result.toString();
}
void main() {
  // Test case 1: Normal case
  assert(expandString('abcs-w1234-9s-4zz', 1, 2, 1) == 'abcsttuuvvw1234556677889s-4zz');

  // Test case 2: With numbers, order preserved
  assert(expandString('3-6', 1, 1, 1) == '3456');

  // Test case 3: Mixed characters, inverse order with stars
  assert(expandString('a-d-d', 2, 3, 2) == 'aCCCBBBd-d');
}