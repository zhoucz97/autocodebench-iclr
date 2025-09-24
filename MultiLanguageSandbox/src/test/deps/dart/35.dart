
List<List<int>> choirFormation(List<List<int>> grades)
/*
Sort students based on their total scores from three subjects (Chinese, Mathematics, and English). 
If students have the same total score, sort by their Chinese grade. If they also have the same Chinese grade, 
sort by their student number. Return the top 5 students' numbers and their total scores.

## Input Format

The input is a list of lists where each sublist contains three integers representing the grades for Chinese, Mathematics, and English.

## Output Format

The output is a list of 5 sublists, each containing two integers: the student number (1-based) and the total score.

## Example

>>> choirFormation([[90,67,80],[87,66,91],[78,89,91],[88,99,77],[67,89,64],[78,89,98]]) => [[6,265],[4,264],[3,258],[2,244],[1,237]]
*/
{
  // Create a list of students with their original indices (student numbers)
  List<Map<String, int>> students = [];
  for (int i = 0; i < grades.length; i++) {
    students.add({
      'studentNumber': i + 1, // 1-based index
      'chinese': grades[i][0],
      'mathematics': grades[i][1],
      'english': grades[i][2],
      'total': grades[i][0] + grades[i][1] + grades[i][2],
    });
  }

  // Sort the students based on the criteria:
  // 1. Total score (descending)
  // 2. Chinese grade (descending)
  // 3. Student number (ascending)
  students.sort((a, b) {
    if (b['total'] != a['total']) {
      return b['total'] - a['total'];
    } else if (b['chinese'] != a['chinese']) {
      return b['chinese'] - a['chinese'];
    } else {
      return a['studentNumber'] - b['studentNumber'];
    }
  });

  // Take the top 5 students and format the output
  List<List<int>> result = [];
  for (int i = 0; i < 5 && i < students.length; i++) {
    result.add([students[i]['studentNumber'], students[i]['total']]);
  }

  return result;
}
void main() {
  testChoirFormation();
}

void testChoirFormation() {
  assert(
    choirFormation([
      [90,67,80], 
      [87,66,91], 
      [78,89,91], 
      [88,99,77], 
      [67,89,64], 
      [78,89,98]
    ]).toString() == 
    [[6,265],[4,264],[3,258],[2,244],[1,237]].toString()
  );

  assert(
    choirFormation([
      [70, 60, 80], 
      [80, 70, 60], 
      [60, 70, 80], 
      [70, 80, 90], 
      [50, 50, 50], 
      [100, 100, 100]
    ]).toString() == 
    [[6, 300], [4, 240], [2, 210], [1, 210], [3, 210]].toString()
  );

  assert(
    choirFormation([
      [80, 80, 80], 
      [80, 80, 80], 
      [80, 80, 80], 
      [80, 80, 80], 
      [80, 80, 80], 
      [80, 80, 80]
    ]).toString() == 
    [[1, 240], [2, 240], [3, 240], [4, 240], [5, 240]].toString()
  );
}