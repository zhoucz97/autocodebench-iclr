import java.util.regex.Pattern
/**
    * Extracts all integer numbers from a given string and returns them as a sorted list.
    * The integers in the string are assumed to be non-negative and separated by non-digit characters.
    * If no integers are found, returns a list containing only zero.
    *
    * Example:
    * >>> extractAndSortIntegers("*1234.345#6781ad9jk81-11101?aght88ir09kp")
    *     [9, 9, 81, 88, 345, 1234, 6781, 11101]
    * >>> extractAndSortIntegers("no numbers")
    *     [0]
**/

fun extractAndSortIntegers(input: String): List<Int> 
{
    // Use regular expression to find all sequences of digits in the string
    val pattern = Pattern.compile("\\d+")
    val matcher = pattern.matcher(input)
    
    val integers = mutableListOf<Int>()
    
    while (matcher.find()) {
        val numberStr = matcher.group()
        try {
            val number = numberStr.toInt()
            integers.add(number)
        } catch (e: NumberFormatException) {
            // Handle cases where the number is too large to fit in an Int (though unlikely with the problem constraints)
            continue
        }
    }
    
    return if (integers.isEmpty()) {
        listOf(0)
    } else {
        integers.sorted()
    }
}
check(extractAndSortIntegers("*1234.345#6781ad9jk81-11101?aght88ir09kp") == listOf(9, 9, 81, 88, 345, 1234, 6781, 11101))
check(extractAndSortIntegers("no numbers") == listOf(0))
check(extractAndSortIntegers("123&456*789") == listOf(123, 456, 789))
check(extractAndSortIntegers("abc") == listOf(0))