import 'dart:math';
List<int> calculateNewDigits(int N, int M, List<int> digits)
/**
A Martian demonstrated their counting technique using a human hand. If the five fingers—thumb, index finger, middle finger, ring finger, and little finger—are numbered $1,2,3,4$, and $5$ respectively, they form the $5$-digit number $12345$ when arranged in the typical order. Swapping the positions of the ring and little fingers results in the $5$-digit number $12354$. Completely reversing the order of the five fingers yields $54321$. Among all $120$ possible $5$-digit numbers that can be formed, $12345$ is the smallest and represents $1$; $12354$ is the second smallest, representing $2$; $54321$ is the largest, representing $120$. The following table displays the $6$ possible $3$-digit numbers that can be formed with just $3$ fingers and the numerals they represent:

| Ternary Number | Represented Digit |
|:-:|:-:|
| $123$ | $1$ |
| $132$ | $2$ |
| $213$ | $3$ |
| $231$ | $4$ |
| $312$ | $5$ |
| $321$ | $6$ |
A Martian will show you their fingers, and the scientists will inform you of the small number to be added. Your task is to add the number represented by the Martian's fingers to the number given by the scientists and rearrange the Martian fingers according to the sum. The input data guarantees that the result will not exceed the range representable by Martian fingers.

## Input Format
There are three lines in total.  
The first line contains a positive integer $N$, indicating the number of Martian fingers 
The second line contains a positive integer $M$, representing the small integer to be added 
The next line is a permutation of the integers from $1$ to $N$, separated by spaces, indicating the order of the Martian fingers.

## Output Format
$N$ integers, representing the rearranged order of Martian fingers after the change. Each pair of adjacent numbers is separated by a space, with no superfluous spaces allowed.

example:
calculateNewDigits(3, 123, [1, 2, 3]).join() => '246'
*/
{
  // Step 1: Convert the current permutation to its rank (1-based)
  int rank = permutationToRank(digits);
  
  // Step 2: Add M to the rank
  int newRank = rank + M;
  
  // Step 3: Convert the new rank back to a permutation
  List<int> newPermutation = rankToPermutation(N, newRank);
  
  return newPermutation;
}

int permutationToRank(List<int> permutation) {
  int rank = 1;
  int n = permutation.length;
  List<int> numbers = List.from(permutation);
  
  for (int i = 0; i < n; i++) {
    int current = numbers[i];
    int count = 0;
    
    for (int j = i + 1; j < n; j++) {
      if (numbers[j] < current) {
        count++;
      }
    }
    
    rank += count * factorial(n - 1 - i);
  }
  
  return rank;
}

List<int> rankToPermutation(int N, int rank) {
  List<int> numbers = List.generate(N, (index) => index + 1);
  List<int> permutation = [];
  rank--; // convert to 0-based index
  
  for (int i = 0; i < N; i++) {
    int fact = factorial(N - 1 - i);
    int index = rank ~/ fact;
    permutation.add(numbers.removeAt(index));
    rank %= fact;
  }
  
  return permutation;
}

int factorial(int n) {
  if (n <= 1) return 1;
  return n * factorial(n - 1);
}
void testCalculateNewDigits() {
  assert(calculateNewDigits(3, 123, [1, 2, 3]).join() == '246');
  assert(calculateNewDigits(4, 0, [1, 0, 0, 0]).join() == '1000');
  assert(calculateNewDigits(2, 99, [3, 1]).join() == '130');

  print('All tests passed!');
}


void main() {
  testCalculateNewDigits();
}