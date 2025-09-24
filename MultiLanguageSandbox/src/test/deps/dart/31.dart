
int findMostUnhappyDay(List<List<int>> schedule)
/**
xx has entered junior high school. Her mother believes that xx should be more dedicated to her studies; therefore, aside from attending school, xx is also enrolled in various subject review classes by her mother. Moreover, her mother sends her to learn recitation, dance, and piano every week. However, if xx's daily class duration exceeds eight hours, she becomes unhappy, and the longer the duration, the more her discontent grows. Assuming xx's discontent arises from no other cause and does not carry over to the next day, please help check xx's schedule for the coming week to determine whether she will be unhappy; if so, identify the day she will be most discontent.

## Input Format

The input consists of $7$ lines of data, representing the schedule from Monday to Sunday. Each line includes two non-negative integers less than $10$, separated by a space, representing the time xx spends in school and the time arranged by her mother for classes, respectively.

## Output Format

A single digit. Output $0$ if she will not be discontent; otherwise, output the day of the week she will be most discontent (with $1, 2, 3, 4, 5, 6, 7$ representing Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, respectively). If the degree of discontent is equivalent on two or more days, output the earliest day.

example
findMostUnhappyDay([[5, 3],[6, 2],[7, 2],[5, 3],[5, 4],[0, 4], [0, 6]]) => 3
*/
{
  int maxUnhappiness = -1;
  int mostUnhappyDay = 0;
  
  for (int day = 0; day < schedule.length; day++) {
    int schoolHours = schedule[day][0];
    int extraHours = schedule[day][1];
    int totalHours = schoolHours + extraHours;
    
    if (totalHours > 8) {
      int unhappiness = totalHours - 8;
      if (unhappiness > maxUnhappiness) {
        maxUnhappiness = unhappiness;
        mostUnhappyDay = day + 1; // days are 1-based in output
      } else if (unhappiness == maxUnhappiness) {
        // prefer earlier day, so do nothing as it's already the first occurrence
      }
    }
  }
  
  return mostUnhappyDay;
}
void testFindMostUnhappyDay() {
  // Test case 1
  assert(findMostUnhappyDay([
    [5, 3],
    [6, 2],
    [7, 2],
    [5, 3],
    [5, 4],
    [0, 4],
    [0, 6]
  ]) == 3);

  // Test case 2
  assert(findMostUnhappyDay([
    [2, 3],
    [1, 2],
    [2, 2],
    [3, 5],
    [6, 2],
    [5, 3],
    [4, 4]
  ]) == 0);

  // Test case 3
  assert(findMostUnhappyDay([
    [1, 1],
    [1, 1],
    [1, 1],
    [1, 1],
    [1, 1],
    [1, 7],
    [8, 1]
  ]) == 7);
}

void main() {
  testFindMostUnhappyDay();
}