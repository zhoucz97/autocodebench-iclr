
int findEarliestTime(int N, int M, List<Road> roads)
/*
Following an earthquake in Region A, the highways connecting all the villages were damaged and became impassable. The government has dispatched crews to repair these highways.
Given the number of villages $N$ in Region A and the number of highways $M$, which are bidirectional, you are provided with information about which two villages each highway connects, and the time it will take to repair each highway. The question is: at the earliest, when will any two villages be accessible by car? That is, when will there be at least one repaired road between every pair of villages (where a road may consist of multiple highways)?

## Input Format

The first line contains two positive integers $N, M$.

The following $M$ lines, each containing three positive integers $x, y, t$, inform you that the highway connects villages $x$ and $y$, and can be repaired by time $t$.

## Output Format

If after repairing all highways there still exist two villages that cannot be accessed by car, output $-1$. Otherwise, output the earliest time at which any two villages can be accessed by car.

## Sample Case #1
findEarliestTime(4, 4, [
  Road(1, 2, 6),
  Road(1, 3, 4),
  Road(1, 4, 5),
  Road(4, 2, 3),
]) => 5
*/
{
  final int x;
  final int y;
  final int t;

  Road(this.x, this.y, this.t);
}

class UnionFind {
  late List<int> parent;
  late List<int> rank;
  int count;

  UnionFind(int size) {
    parent = List<int>.generate(size + 1, (index) => index); // 1-based indexing
    rank = List<int>.filled(size + 1, 1);
    count = size;
  }

  int find(int p) {
    if (parent[p] != p) {
      parent[p] = find(parent[p]);
    }
    return parent[p];
  }

  bool union(int p, int q) {
    int rootP = find(p);
    int rootQ = find(q);
    if (rootP == rootQ) {
      return false;
    }
    if (rank[rootP] > rank[rootQ]) {
      parent[rootQ] = rootP;
    } else if (rank[rootP] < rank[rootQ]) {
      parent[rootP] = rootQ;
    } else {
      parent[rootQ] = rootP;
      rank[rootP]++;
    }
    count--;
    return true;
  }
}

int findEarliestTime(int N, int M, List<Road> roads) {
  // Sort roads by repair time in ascending order
  roads.sort((a, b) => a.t.compareTo(b.t));

  UnionFind uf = UnionFind(N);
  int maxTime = 0;

  for (Road road in roads) {
    int x = road.x;
    int y = road.y;
    int t = road.t;

    if (uf.find(x) != uf.find(y)) {
      uf.union(x, y);
      maxTime = t;
      if (uf.count == 1) {
        break;
      }
    }
  }

  return uf.count == 1 ? maxTime : -1;
}
class Road {
  int village1;
  int village2;
  int repairTime;

  Road(this.village1, this.village2, this.repairTime);

  @override
  String toString() => 'Road(village1: $village1, village2: $village2, repairTime: $repairTime)';
}

void testFindEarliestTime() {
  assert(findEarliestTime(4, 4, [
    Road(1, 2, 6),
    Road(1, 3, 4),
    Road(1, 4, 5),
    Road(4, 2, 3)
  ]) == 5);

  assert(findEarliestTime(3, 3, [
    Road(1, 2, 2),
    Road(2, 3, 5),
    Road(1, 3, 1)
  ]) == 2);

  assert(findEarliestTime(5, 3, [
    Road(1, 2, 3),
    Road(2, 3, 4),
    Road(4, 5, 5)
  ]) == -1);
}

void main() {
  testFindEarliestTime();
}