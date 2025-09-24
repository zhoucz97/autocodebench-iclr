
import 'dart:math';

int calculateMinimumTime(List<List<int>> tasks, List<int> timeToComplete)
/*
Prior to milking the dairy cows on John's farm, there are numerous miscellaneous tasks that need to be completed, each requiring a certain amount of time for execution. 

Naturally, some tasks can only be initiated upon the completion of other specific tasks.There is at least one task that does not require any preparatory work, which can be the first task to be tackled, designated as task $1$.

John possesses a list of $n$ miscellaneous tasks that require completion, and this list is structured in a particular order, with the preparatory work for task $k\ (k>1)$ only possibly being among tasks $1$ to $k-1$.

Write a program to sequentially read in the description of each task. Compute the shortest time required to complete all tasks. It is understood that tasks unrelated to one another can be executed concurrently, and it can be assumed that John's farm is sufficiently staffed to simultaneously accomplish any number of tasks.

## Input Format

tasks: A list of $n$ tasks, each represented by a list of integers, where the first integer is the task number, the second integer is the time required to complete the task, and the remaining integers are the prerequisite tasks that must be completed beforehand, ending with the digit $0$.

timeToComplete: A list of $n$ integers, where the $i$-th integer is the time required to complete the $i$-th task.

## Output Format

A single integer representing the shortest time required to complete all miscellaneous tasks.

## Sample Case #1
calculateMinimumTime([[1, 5, 0], [2, 2, 1, 0], [3, 3, 2, 0], [4, 6, 1, 0], [5, 1, 2, 4, 0], [6, 8, 2, 4, 0], [7, 4, 3, 5, 6, 0]]) => 23
*/
{
  int n = tasks.length;
  // Create a map to store the prerequisites for each task (task number -> list of prerequisites)
  Map<int, List<int>> graph = {};
  // Create a map to store the completion time of each task
  Map<int, int> completionTime = {};
  
  // Initialize completion times to 0 (tasks without prerequisites can start at time 0)
  for (var task in tasks) {
    int taskNumber = task[0];
    completionTime[taskNumber] = 0;
  }
  
  // Process each task to build the graph and compute completion times
  for (var task in tasks) {
    int taskNumber = task[0];
    int duration = task[1];
    List<int> prerequisites = [];
    int i = 2;
    while (task[i] != 0) {
      prerequisites.add(task[i]);
      i++;
    }
    
    // For each prerequisite, ensure it's in the graph (though it might be added later)
    int maxPrerequisiteTime = 0;
    for (int prereq in prerequisites) {
      if (completionTime.containsKey(prereq)) {
        if (completionTime[prereq] > maxPrerequisiteTime) {
          maxPrerequisiteTime = completionTime[prereq]!;
        }
      }
    }
    
    // The start time of the current task is the max completion time of its prerequisites
    int startTime = maxPrerequisiteTime;
    int endTime = startTime + duration;
    completionTime[taskNumber] = endTime;
  }
  
  // The answer is the maximum completion time among all tasks
  int maxTime = 0;
  completionTime.forEach((taskNumber, time) {
    if (time > maxTime) {
      maxTime = time;
    }
  });
  
  return maxTime;
}
void testCalculateMinimumTime() {
  assert(calculateMinimumTime([
    [],
    [0],
    [1],
    [0],
    [1, 3],
    [1, 3],
    [2, 4, 5]
  ],
  [5, 2, 3, 6, 1, 8, 4]) == 23);

  assert(calculateMinimumTime([
    [],
    [0],
    [1],
    [1, 2]
  ],
  [4, 3, 2, 5]) == 14);

  assert(calculateMinimumTime([
    [],
    [0],
    [0],
    [0],
    [1, 2, 3]
  ],
  [1, 2, 3, 4, 5]) == 10);
}

void main() {
  testCalculateMinimumTime();
}