

/**
 * Sorts an array of names in alphabetical order.
 * This function takes an array of names as an input and returns a new array with the names sorted in alphabetical order.
 * It is assumed that each name is a non-empty string consisting of alphabetic characters.
 *
 * Example:
 * >>> sortNames(Array("Apple", "Tom", "Green", "Jack"))
 * Array("Apple", "Green", "Jack", "Tom")
 */
 object Solution {
 def sortNames(names: Array[String]): Array[String] = {
  names.sorted
}
  def main(args: Array[String]): Unit = {
    val names = Array("Apple", "Tom", "Green", "Jack")
    val sortedNames = sortNames(names)
    assert(sortedNames.sameElements(Array("Apple", "Green", "Jack", "Tom")))

    val moreNames = Array("Banana", "Zebra", "Elephant", "Crocodile")
    val sortedMoreNames = sortNames(moreNames)
    assert(sortedMoreNames.sameElements(Array("Banana", "Crocodile", "Elephant", "Zebra")))
 }
 }