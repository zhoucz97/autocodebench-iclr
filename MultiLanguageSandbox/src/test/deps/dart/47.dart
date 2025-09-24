
import 'dart:math';

int longestBalancedSubsequence(List<int> students)
/*
As Halloween approaches, `XXX` plans to organize a large-scale "Perfect Pairing" event at his school. For the participants of this event, `XXX` has a unique method of selection. He wishes to choose individuals in equal numbers of males and females whose heights are closely matched. The mechanism for this selection is straightforward: he has all the school's students line up by height and then selects a continuous group from this line such that the number of males and females is equal. To ensure the event is lively, it goes without saying that `XXX` prefers to select as many individuals as possible. Please write a program to inform him of the maximum number of individuals he can select.

## Input Format

students: an array of integers representing the heights of the students in the school. The length of the array is between 1 and 10^5. Each element of the array is an integer between 0 and 1.

## Output Format

Output a non-negative integer. This number represents the length of the longest subinterval with an equal number of males and females in the input data.

If there are no subintervals with an equal number of males and females, please output $0$.

## Sample Case #1
longestBalancedSubsequence([0, 1, 0, 0, 0, 1, 1, 0, 0]) => 6
*/
{
  Map<int, int> balanceIndices = {};
  balanceIndices[0] = -1;
  int maxLen = 0;
  int balance = 0;
  
  for (int i = 0; i < students.length; i++) {
    if (students[i] == 1) {
      balance++;
    } else {
      balance--;
    }
    
    if (balanceIndices.containsKey(balance)) {
      int prevIndex = balanceIndices[balance]!;
      int currentLen = i - prevIndex;
      if (currentLen > maxLen) {
        maxLen = currentLen;
      }
    } else {
      balanceIndices[balance] = i;
    }
  }
  
  return maxLen;
}
void testLongestBalancedSubsequence() {
  assert(longestBalancedSubsequence([0, 1, 0, 0, 0, 1, 1, 0, 0]) == 6);
  assert(longestBalancedSubsequence([1, 1, 0, 0, 0, 1, 0, 1, 1]) == 8);
  assert(longestBalancedSubsequence([0, 1, 1, 0, 1, 1, 0, 0]) == 8);
}

void main() {
  testLongestBalancedSubsequence();
}