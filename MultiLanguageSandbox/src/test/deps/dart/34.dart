import 'dart:collection';
int minEnergy(List<int> weights)
/*
Within an orchard, BB has harvested all the fruits and sorted them into different piles according to their varieties. BB has decided to merge all the fruits into a single pile.

Each merge allows BB to combine two piles of fruits together, with the expenditure of physical energy equivalent to the sum of the weights of the two piles. It is evident that after $n-1$ merges, there will remain only one pile. The total amount of energy expended by BB in merging the fruits is equal to the sum of the energy consumed in each merge.

Assuming that each fruit weighs $1$ and knowing the number of fruit varieties and the quantity of each variety, your task is to devise a merging strategy that minimizes BB's energy expenditure and output this minimum value.
ue.

## Input Format
The second line contains $n$ integers, separated by spaces, where the $i$-th integer $a_i$ represents the quantity of the $i$-th variety of fruit.

## Output Format

A single integer, which is the minimum value of the energy expenditure. The input data guarantees that this value is less than $2^{31}$.

## Sample
minEnergy([1, 2, 9]) => 15
*/
{
  if (weights.length == 1) return 0;
  
  var heap = HeapPriorityQueue<int>();
  for (var weight in weights) {
    heap.add(weight);
  }
  
  int totalEnergy = 0;
  
  while (heap.length > 1) {
    int first = heap.removeFirst();
    int second = heap.removeFirst();
    int sum = first + second;
    totalEnergy += sum;
    heap.add(sum);
  }
  
  return totalEnergy;
}
void testMinEnergy() {
  assert(minEnergy([1, 2, 9]) == 15);
  assert(minEnergy([1, 2, 3, 4]) == 19);
  assert(minEnergy([10]) == 0);
}

void main() {
  testMinEnergy();
}