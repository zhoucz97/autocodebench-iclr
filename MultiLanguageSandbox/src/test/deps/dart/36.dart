
int minimumGroups(List<int> gifts, int limit)
/*
the school's student council has assigned BB the task of distributing souvenirs for the New Year's Eve party. To ensure that the value of souvenirs received by the students attending the party is relatively balanced, BB must group the purchased souvenirs by price. However, each group can contain at most two souvenirs, and the total price of the souvenirs in each group must not exceed a given integer. To guarantee that all souvenirs are distributed in the shortest possible time, BB hopes to minimize the number of groups.

Your task is to write a program that finds the least number of groups among all possible grouping schemes and outputs this minimum number.

## Input Format

There are $n+1$ lines in total:

The first line includes an integer $w$, which is the upper limit for the total price of souvenirs in each group.

Lines $2$ to $n+1$ each contain a positive integer $P_i$ representing the price of the corresponding souvenir.

## Output Format

A single integer, which is the minimum number of groups.

## Sample #1

minimumGroups([90, 20, 20, 30, 50, 60, 70, 80, 90], 100) => 6
*/
{
  gifts.sort();
  int left = 0;
  int right = gifts.length - 1;
  int groups = 0;
  
  while (left <= right) {
    if (gifts[left] + gifts[right] <= limit) {
      left++;
    }
    right--;
    groups++;
  }
  
  return groups;
}
void testMinimumGroups() {
  assert(minimumGroups([90, 20, 20, 30, 50, 60, 70, 80, 90], 100) == 6);
  assert(minimumGroups([10, 20, 30, 40, 50, 60, 70, 80, 90], 150) == 5);
  assert(minimumGroups([30, 30, 50, 60, 70], 100) == 3);
}

void main() {
  testMinimumGroups();
}