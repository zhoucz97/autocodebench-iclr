
/**
    * Extracts a specific 4-bit sequence from the binary representation of a given positive decimal integer,
    * starting from the 4th bit to the 7th bit (counting from 0 from the right), and then returns its decimal value.
    Examples:
    >>> extractAndConvertToDecimal(173) // Binary of 173 is 10101101, bits 4-7 are 1010
        10
    >>> extractAndConvertToDecimal(59) // Binary of 59 is 111011, bits 4-7 are 0011
        3
**/

fun extractAndConvertToDecimal(number: Int): Int 
{
    // Mask to get bits 4-7: 0xF0 (binary 11110000)
    val mask = 0xF0
    // Apply mask and shift right by 4 to get the 4-bit value
    return (number and mask) shr 4
}
fun main(){
    check(extractAndConvertToDecimal(217) == 13)  // 173 in binary is 10101101, bits 4-7 are 1101
    check(extractAndConvertToDecimal(173) == 10)  // 173 in binary is 10101101, bits 4-7 are 1010
    check(extractAndConvertToDecimal(59) == 3)    // 59 in binary is 111011, bits 4-7 are 0011
    check(extractAndConvertToDecimal(32) == 2)    // 32 in binary is 100000, bits 4-7 are 0010
    check(extractAndConvertToDecimal(255) == 15)  // 255 in binary is 11111111, bits 4-7 are 1111
    check(extractAndConvertToDecimal(1) == 0)     // 1 in binary is 1, bits 4-7 are 0000

}

main()