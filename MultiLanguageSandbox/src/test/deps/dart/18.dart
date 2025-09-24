
List<List<int>> sortAndFilterApplicants(List<List<int>> applicants, int m)
/**
Sorts and filters a list of applicants based on their scores and an interview cutoff line.

The function accepts a list of applicant records, where each record is a list containing
two integers: the applicant's number and their examination score. The function sorts the 
applicants primarily by score in descending order and secondarily by applicant number in 
ascending order. It then filters the applicants to include only those whose scores meet 
or exceed the score of the applicant at the interview cutoff line, which is determined 
as the score of the applicant ranked at position `m* 1.5` (floored). The final list
contains all applicants who have made it past the cutoff line.

@param applicants A list of lists where each inner list contains two integers representing
                  an applicant's number and score, respectively.
@param m          An integer representing the number of positions available for interview.

@return A list of applicants who have scores greater than or equal to the interview line score.

Example cases:
sortAndFilterApplicants([[1, 90], [2, 85], [3, 80]],2) 
return [[1, 90], [2, 85], [3, 80]]
*/
{
  // Sort applicants primarily by score descending, then by applicant number ascending
  applicants.sort((a, b) {
    if (b[1] != a[1]) {
      return b[1].compareTo(a[1]); // Higher score first
    } else {
      return a[0].compareTo(b[0]); // Lower applicant number first if scores are equal
    }
  });

  // Calculate the cutoff position (m * 1.5 floored)
  int cutoffPosition = (m * 1.5).floor();
  
  // If there are not enough applicants, return all sorted applicants
  if (cutoffPosition >= applicants.length) {
    return applicants;
  }

  // Get the score of the applicant at the cutoff position
  int cutoffScore = applicants[cutoffPosition][1];

  // Filter applicants with score >= cutoffScore
  List<List<int>> filteredApplicants = [];
  for (var applicant in applicants) {
    if (applicant[1] >= cutoffScore) {
      filteredApplicants.add(applicant);
    } else {
      break; // Since they're sorted, we can stop when we find the first below cutoff
    }
  }

  return filteredApplicants;
}
void testSortAndFilterApplicants() {
  // Test case 1: Basic test with already sorted applicants
  List<List<int>> applicants1 = [
    [1, 90],
    [2, 85],
    [3, 80],
  ];
  List<List<int>> expected1 = [[1, 90], [2, 85], [3, 80]];
  assert(sortAndFilterApplicants(applicants1, 2).toString() == expected1.toString());

  // Test case 2: Test with unsorted applicants and a cutoff that excludes some applicants
  List<List<int>> applicants2 = [
    [3, 70],
    [1, 95],
    [2, 90],
  ];
  List<List<int>> expected2 = [[1, 95]];
  assert(sortAndFilterApplicants(applicants2, 1).toString() == expected2.toString());

  // Test case 3: Test with a tie on scores
  List<List<int>> applicants3 = [
    [5, 85],
    [2, 85],
    [1, 90],
    [3, 80],
    [4, 85],
  ];
  List<List<int>> expected3 = [
    [1, 90],
    [2, 85],
    [4, 85],
    [5, 85],
  ];
  assert(sortAndFilterApplicants(applicants3, 2).toString() == expected3.toString());

  print('All test cases passed!');
}

void main() {
  testSortAndFilterApplicants();
}