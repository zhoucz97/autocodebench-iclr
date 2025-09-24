
int calculateScholarship(  String name, int finalScore, int classScore, bool isCadre, bool isWestern, int papers)
/*Calculates the scholarship amount a student is eligible to receive based on
academic performance, leadership, geographic background, and research contributions.

The scholarship is calculated by considering the student's final exam score,
class score, whether they are a student cadre (a leadership position),
whether they come from a western region, and the number of papers they have
published. Different conditions contribute varying amounts to the total scholarship.

Args:
  name (String): The name of the student (not used in the calculation).
  finalScore (int): The student's final exam score.
  classScore (int): The student's class score.
  isCadre (bool): Indicates if the student is a student cadre (true/false).
  isWestern (bool): Indicates if the student is from a western region (true/false).
  papers (int): The number of papers the student has published.

Returns:
  int: The total amount of scholarship money the student is eligible to receive.

Examples:
  calculateScholarship("Alice", 90, 85, false, false, 2) returns 14000
  calculateScholarship("Charlie", 75, 75, false, false, 0) returns 0
*/
{
  int scholarship = 0;

  // Academic performance calculation
  if (finalScore >= 90 && classScore >= 85) {
    scholarship += 10000;
  } else if (finalScore >= 80 && classScore >= 80) {
    scholarship += 8000;
  } else if (finalScore >= 70 && classScore >= 75) {
    scholarship += 6000;
  } else if (finalScore >= 60 && classScore >= 70) {
    scholarship += 4000;
  }

  // Leadership bonus (student cadre)
  if (isCadre) {
    scholarship += 2000;
  }

  // Geographic background bonus (western region)
  if (isWestern) {
    scholarship += 1000;
  }

  // Research contribution (papers published)
  if (papers > 0) {
    scholarship += papers * 1000;
  }

  return scholarship;
}
void testCalculateScholarship() {
  // Test case 1: Student with high final score, class score, and has papers, but is not a cadre or from the western region
  assert(calculateScholarship("Alice", 90, 85, false, false, 2) == 12000,
    "Test Case 1 Failed: Alice should receive a scholarship of 14000.");

  // Test case 2: Student with high final score, class score, is a cadre, and from the western region, but does not have papers
  assert(calculateScholarship("Bob", 86, 82, true, true, 0) == 5850,
    "Test Case 2 Failed: Bob should receive a scholarship of 5850.");

  // Test case 3: Student with average scores, not a cadre, not from the western region, and no papers
  assert(calculateScholarship("Charlie", 75, 75, false, false, 0) == 0,
    "Test Case 3 Failed: Charlie should not receive any scholarship.");

  print("All calculateScholarship test cases passed!");
}

void main() {
  testCalculateScholarship();
}