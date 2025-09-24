import 'dart:math';
int countPrimeSumCombinations(List<int> numbers, int n, int k)
/*Problem Description

Given are $n$ integers $x_1,x_2,\cdots,x_n$, along with one integer $k$ where $k<n$. By selecting any $k$ integers from these $n$ integers and summing them, a series of sums can be obtained. For instance, when $n=4$, $k=3$, and the four integers are $3,7,12,19$, the complete set of combinations and their sums are as follows:
$3+7+12=22$
$3+7+19=29$
$7+12+19=38$
$3+12+19=34$
The task is to compute the number of combinations for which the sum is a prime number.
For the example given, there is only one combination where the sum is prime: $3+7+19=29$.
example
countPrimeSumCombination([3,7,12,19],4,3) = 1
*/
{
  if (num <= 1) return false;
  if (num == 2) return true;
  if (num % 2 == 0) return false;
  for (int i = 3; i * i <= num; i += 2) {
    if (num % i == 0) return false;
  }
  return true;
}

void _generateCombinations(List<int> numbers, int start, int k, List<int> current, List<List<int>> result) {
  if (current.length == k) {
    result.add(List.from(current));
    return;
  }
  for (int i = start; i < numbers.length; i++) {
    current.add(numbers[i]);
    _generateCombinations(numbers, i + 1, k, current, result);
    current.removeLast();
  }
}

int countPrimeSumCombinations(List<int> numbers, int n, int k) {
  List<List<int>> combinations = [];
  _generateCombinations(numbers, 0, k, [], combinations);
  
  int count = 0;
  for (var combo in combinations) {
    int sum = combo.reduce((a, b) => a + b);
    if (isPrime(sum)) {
      count++;
    }
  }
  return count;
}
void main() {
  // 测试用例1
  {
    List<int> numbers = [3, 7, 12, 19];
    int n = 4;
    int k = 3;
    int expected = 1;
    int result = countPrimeSumCombinations(numbers, n, k);
    assert(result == expected, '测试用例1失败: 预期$expected, 实际$result');
  }

  // 测试用例2
  {
    List<int> numbers = [2, 3, 5, 7];
    int n = 4;
    int k = 2;
    int expected = 2; // 2+3=5, 5+7=12(不是素数)
    int result = countPrimeSumCombinations(numbers, n, k);
    assert(result == expected, '测试用例2失败: 预期$expected, 实际$result');
  }

  // 测试用例3
  {
    List<int> numbers = [1, 3, 6, 8, 10];
    int n = 5;
    int k = 3;
    int expected = 4; // 没有组合的和是素数
    int result = countPrimeSumCombinations(numbers, n, k);
    assert(result == expected, '测试用例3失败: 预期$expected, 实际$result');
  }

  // 测试用例4
  {
    List<int> numbers = [11, 13, 17, 19, 23];
    int n = 5;
    int k = 4;
    int expected = 0; // 所有可能的组合和都是素数
    int result = countPrimeSumCombinations(numbers, n, k);
    assert(result == expected, '测试用例4失败: 预期$expected, 实际$result');
  }

  print('所有测试用例都通过了!');
}