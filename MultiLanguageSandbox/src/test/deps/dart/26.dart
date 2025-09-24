
int getMaxValue(int T, int M, List<List<int>> herbs)

/*
binbin is a child of exceptional talent, aspiring to become the world's greatest physician. To achieve this, he wishes to apprentice himself to the most prestigious physician nearby. To assess his potential, the physician presents him with a challenging task. He takes binbin to a cave filled with various herbs and says, "Child, this cave contains several different herbs, each requiring some time to collect and each possessing its own value. I will give you a certain amount of time, within which you may collect some herbs. If you are a clever child, you should be able to maximize the total value of the herbs you collect."
If you were binbin, could you complete this task?

Input Format

The first line contains $2$ integers $T$ ($1 \le T \le 1000$) and $M$ ($1 \le  M \le 100$), separated by a space, where $T$ represents the total time available for collecting herbs, and $M$ represents the number of herbs in the cave.

The next $M$ lines each include two integers between $1$ and $100$ (inclusive), representing the time required to collect a particular herb and the value of that herb, respectively.

Output Format

Output the maximum total value of the herbs that can be collected within the allotted time.

example
getMaxValue(70, 3, [[71, 100], [69, 1], [1, 2]]) => 3

*/
{
  List<int> dp = List.filled(T + 1, 0);
  
  for (var herb in herbs) {
    int time = herb[0];
    int value = herb[1];
    
    for (int t = T; t >= time; t--) {
      if (dp[t - time] + value > dp[t]) {
        dp[t] = dp[t - time] + value;
      }
    }
  }
  
  return dp[T];
}
void testGetMaxValue() {
  // Test Case 1: Provided example
  assert(getMaxValue(70, 3, [
    [71, 100],
    [69, 1],
    [1, 2]
  ]) == 3);

  // Test Case 2: Multiple herbs can be collected, no single herb takes all time
  assert(getMaxValue(10, 4, [
    [1, 10],
    [2, 15],
    [3, 20],
    [4, 30]
  ]) == 75); // Best option: collect all herbs

  // Test Case 3: Only one herb can be collected due to time constraint
  assert(getMaxValue(5, 3, [
    [5, 50],
    [4, 40],
    [3, 30]
  ]) == 50); // Best option: collect the herb worth 50

  print("All test cases passed successfully.");
}

void main() {
  testGetMaxValue();
}