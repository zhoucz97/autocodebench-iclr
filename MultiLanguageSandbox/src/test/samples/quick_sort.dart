bool sequenceEqual(List<int> a, List<int> b) {
  if (a.length != b.length) return false;
  for (int i = 0; i < a.length; i++) {
    if (a[i] != b[i]) return false;
  }
  return true;
}

List<int> quicksort(List<int>? list) {
  if (list == null || list.length <= 1)
    return list ?? <int>[];
  
  int pivot = list[0];
  List<int> rest = list.skip(1).toList();
  List<int> less = rest.where((x) => x < pivot).toList();
  List<int> greater = rest.where((x) => x >= pivot).toList();
  
  return [...quicksort(less), ...([pivot]), ...quicksort(greater)];
}

void checkQuicksort() {
  assert(sequenceEqual(quicksort([3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]), [1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 9]));
  assert(sequenceEqual(quicksort([5, 4, 3, 2, 1]), [1, 2, 3, 4, 5]));
  assert(sequenceEqual(quicksort([]), []));
  assert(sequenceEqual(quicksort([1]), [1]));
}

void main() {
  checkQuicksort();
}
