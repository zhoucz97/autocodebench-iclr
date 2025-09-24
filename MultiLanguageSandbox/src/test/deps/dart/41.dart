
int minimumUntidiness(List<Book> books, int k)

/*
BB possesses an extensive collection of books and a bookshelf on which he desires to place them. The bookshelf can accommodate the entire collection, leading BB to initially arrange the books in ascending order by height. However, BB discerns that, due to the varying widths of many books, the arrangement still appears untidy. Consequently, he decides to remove $k$ books to potentially achieve a neater appearance.

The untidiness of the bookshelf is defined as follows: it is the sum of the absolute differences in width between every pair of adjacent books. Consider a set of $4$ books:

$1 \times 2$  
$5 \times 3$  
$2 \times 4$  
$3 \times 1$    

Upon arranging the books tidily, the sequence is:

$1 \times 2$  
$2 \times 4$  
$3 \times 1$  
$5 \times 3$  

Here, the untidiness is calculated as $2+3+2=7$.

Given that each book has a unique height, you are tasked with determining the minimum untidiness achievable upon the removal of $k$ books.

## Input Format
books: A list of $n$ books, where each book is represented as a tuple $(h, w)$, where $h$ is the height and $w$ is the width.
k: An integer representing the number of books to remove.

## Output Format

A single integer on one line, representing the minimum untidiness of the bookshelf.

## Sample #1
mimimumUntidiness([(1, 2), (2, 4), (3, 1), (5, 3)], 1) => 3
*/
{
  // First, sort the books by height in ascending order
  books.sort((a, b) => a.height.compareTo(b.height));
  
  int n = books.length;
  if (n == 0 || k >= n) return 0; // If all books are removed, untidiness is 0
  
  // DP table: dp[i][j] represents the min untidiness for first i books with j removals
  List<List<int>> dp = List.generate(n + 1, (_) => List.filled(k + 1, 1000000000)); // Initialize with a large number
  dp[0][0] = 0;
  
  for (int i = 1; i <= n; i++) {
    for (int j = 0; j <= k; j++) {
      // Option 1: do not take the i-th book (1-based), so j must be at least 1
      if (j > 0) {
        dp[i][j] = dp[i - 1][j - 1];
      }
      // Option 2: take the i-th book, then compare with the last taken book
      for (int l = 0; l < i - 1; l++) {
        if (j <= k) {
          int prevWidth = books[l].width;
          int currentWidth = books[i - 1].width;
          int diff = (currentWidth - prevWidth).abs();
          if (dp[l + 1][j] + diff < dp[i][j]) {
            dp[i][j] = dp[l + 1][j] + diff;
          }
        }
      }
    }
  }
  
  // The answer is the minimal value in dp[n][k]
  int minUntidiness = 1000000000;
  for (int j = 0; j <= k; j++) {
    if (dp[n][j] < minUntidiness) {
      minUntidiness = dp[n][j];
    }
  }
  
  return minUntidiness;
}

class Book {
  final int height;
  final int width;
  
  Book(this.height, this.width);
}
void main() {
  testMinimumUntidiness();
}

void testMinimumUntidiness() {
  assert(minimumUntidiness([
    Book(1, 2),
    Book(2, 4),
    Book(3, 1),
    Book(5, 3)
  ], 1) == -4);

  assert(minimumUntidiness([
    Book(4, 5),
    Book(1, 2),
    Book(3, 2),
    Book(2, 1)
  ], 2) == -7);

  assert(minimumUntidiness([
    Book(1, 10),
    Book(2, 3),
    Book(3, 7),
    Book(4, 1),
    Book(5, 5)
  ], 3) == -22);
}