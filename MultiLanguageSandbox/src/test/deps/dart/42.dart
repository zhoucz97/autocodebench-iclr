
List<String> sortStudentsByBirthday(List<List<String>> inputList)
/*
CJF wishes to survey the birthdays of each student in the school's OI (Olympiad in Informatics) group and sort them in descending order by age. However, CJF is currently overwhelmed with homework and lacks the time, thus your assistance is requested for the sorting task.

## Input Format

inputList is a list of lists, where each inner list contains the name of a student and their birthday in the format of [name, year, month, day].

## Output Format

The output should consist of $n$ lines,

representing the names of the $n$ students sorted from the oldest to the youngest birthday. (If two students share the same birthday, the one whose information was entered later should be output first.)

## Sample #1
sortStudentsByBirthday([["Yangchu", "1992", "4", "23"], ["Qiujingya", "1993", "10", "13"], ["Luowen", "1991", "8", "1"]])
returns ["Luowen", "Yangchu", "Qiujingya"]
*/
{
  // Convert each student's birthday to a DateTime object for easy comparison
  List<Map<String, dynamic>> studentsWithDate = inputList.map((student) {
    String name = student[0];
    int year = int.parse(student[1]);
    int month = int.parse(student[2]);
    int day = int.parse(student[3]);
    DateTime birthday = DateTime(year, month, day);
    return {
      'name': name,
      'birthday': birthday,
      'originalIndex': inputList.indexOf(student), // To handle ties by original order
    };
  }).toList();

  // Sort by birthday in descending order, then by original index in ascending order (to keep later entries first)
  studentsWithDate.sort((a, b) {
    int dateComparison = b['birthday'].compareTo(a['birthday']);
    if (dateComparison != 0) {
      return dateComparison;
    } else {
      // For same birthdays, the one with higher originalIndex comes first (since we want later entries first)
      return b['originalIndex'].compareTo(a['originalIndex']);
    }
  });

  // Extract just the names in the sorted order
  return studentsWithDate.map((student) => student['name']).toList();
}
void main() {
  testSortStudentsByBirthday();
}

void testSortStudentsByBirthday() {
  assert(sortStudentsByBirthday([
    ["Alice", "1990", "5", "12"],
    ["Bob", "1989", "1", "15"],
    ["Charlie", "1990", "5", "12"],
  ]).join(',') == "Alice,Charlie,Bob");

  assert(sortStudentsByBirthday([
    ["David", "1991", "3", "11"],
    ["Eve", "1992", "7", "30"],
    ["Frank", "1991", "3", "11"],
  ]).join(',') == "Eve,David,Frank");

  assert(sortStudentsByBirthday([
    ["George", "1990", "4", "22"],
    ["Hannah", "1992", "10", "13"],
    ["Ian", "1991", "8", "1"],
  ]).join(',') == "Hannah,Ian,George");
}