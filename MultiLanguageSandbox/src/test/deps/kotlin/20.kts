

/**
 * Perform basic arithmetic operations (addition, subtraction, multiplication, division, and modulus)
 * on two integers based on a specified operation character. Illegal calculation returns "invalid calcu"
 * 
 * Example:
 * >>> simpleCalculator(5, 3, '+')
 *     8
 * >>> simpleCalculator(5, 3, '-')
 *     2
 * >>> simpleCalculator(5, 3, '*')
 *     15
 * >>> simpleCalculator(5, 3, '/')
 *     1
 * >>> simpleCalculator(5, 3, '%')
 *     2
 **/
fun simpleCalculator(a: Int, b: Int, operation: Char): Any 
{
    return when (operation) {
        '+' -> a + b
        '-' -> a - b
        '*' -> a * b
        '/' -> if (b != 0) a / b else "invalid calcu"
        '%' -> if (b != 0) a % b else "invalid calcu"
        else -> "invalid calcu"
    }
}
fun main(){
    check(simpleCalculator(10, 5, '+') == 15)
    check(simpleCalculator(10, 5, '-') == 5)
    check(simpleCalculator(10, 5, '*') == 50)
    check(simpleCalculator(10, 5, '/') == 2)
    check(simpleCalculator(10, 5, '%') == 0)
    check(simpleCalculator(15, 4, '/') == 3)
    check(simpleCalculator(15, 4, '%') == 3)
    check(simpleCalculator(15, 0, '/') == "invalid calc")
    check(simpleCalculator(15, 0, '%') == "invalid calc")

}
main()