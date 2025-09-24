
int swapHighLowBits(int n)
/*
Given a non-negative integer less than $2^{32}$, this number can be represented by a 32-bit binary number (if less than 32 bits, zeros are added to make up the difference). We refer to the first 16 bits of this binary number as the "high bits" and the last 16 bits as the "low bits." By swapping the high and low bits, we can obtain a new number. The task is to determine what this new number is in decimal representation.

## Input Format

A non-negative integer less than $2^{32}$.

## Output Format

Output the new number.

## Sample Case #1
swapHighLowBits(1314520) => 249036820
*/
{
  // Extract the high 16 bits by right shifting 16 positions
  int highBits = (n >> 16) & 0xFFFF;
  // Extract the low 16 bits by masking with 0xFFFF
  int lowBits = n & 0xFFFF;
  // Combine the low bits shifted left by 16 with the high bits
  return (lowBits << 16) | highBits;
}
void testSwapHighLowBits() {
  assert(swapHighLowBits(1314520) == 249036820);
  assert(swapHighLowBits(0xFFFFFFFF) == 0xFFFFFFFF);
  assert(swapHighLowBits(0) == 0);
}

void main() {
  testSwapHighLowBits();
}