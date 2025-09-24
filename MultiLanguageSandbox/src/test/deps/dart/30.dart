
int modInverse(int a, int b)
/*
Determine the smallest positive integer solution for the congruence equation $ax \equiv 1 \pmod{b}$ with respect to $x$.

Input Format
A single line containing two integers $a, b$, separated by a space.

Output Format
A single integer $x_0$, representing the smallest positive integer solution. It is guaranteed that a solution exists.

example
modInverse(3, 10) => 7
*/
{
  int originalB = b;
  int x0 = 0, x1 = 1;
  if (b == 1) {
    return 1;
  }
  while (a > 1) {
    int q = a ~/ b;
    int temp = b;
    b = a % b;
    a = temp;
    temp = x0;
    x0 = x1 - q * x0;
    x1 = temp;
  }
  if (x1 < 0) {
    x1 += originalB;
  }
  return x1;
}
void testModInverse() {
  assert(modInverse(3, 10) == 7);
  assert(modInverse(25, 28) == 9);
  assert(modInverse(4, 7) == 2);
}

void main() {
  testModInverse();
}