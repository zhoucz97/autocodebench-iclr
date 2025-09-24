
/* Check if in given list of numbers, any two numbers are closer to each other than
   the given threshold.
   >>> hasCloseElements([1.0, 2.0, 3.0], 0.5)
   false
   >>> hasCloseElements([1.0, 2.8, 3.0, 4.0, 5.0, 2.0], 0.3)
   true
*/
bool hasCloseElements(List<double> numbers, double threshold)
{
  // Sort the list to easily find the closest elements
  numbers.sort();
  
  // Iterate through the sorted list and check adjacent elements
  for (int i = 0; i < numbers.length - 1; i++) {
    if ((numbers[i + 1] - numbers[i]).abs() < threshold) {
      return true;
    }
  }
  
  return false;
}
void main() {
  check(hasCloseElements);
}

void check(bool Function(List<double>, double) hasCloseElements) {
  assert(hasCloseElements([1.0, 2.0, 3.9, 4.0, 5.0, 2.2], 0.3) == true);
  assert(hasCloseElements([1.0, 2.0, 3.9, 4.0, 5.0, 2.2], 0.05) == false);
  assert(hasCloseElements([1.0, 2.0, 5.9, 4.0, 5.0], 0.95) == true);
  assert(hasCloseElements([1.0, 2.0, 5.9, 4.0, 5.0], 0.8) == false);
  assert(hasCloseElements([1.0, 2.0, 3.0, 4.0, 5.0, 2.0], 0.1) == true);
  assert(hasCloseElements([1.1, 2.2, 3.1, 4.1, 5.1], 1.0) == true);
  assert(hasCloseElements([1.1, 2.2, 3.1, 4.1, 5.1], 0.5) == false);
}