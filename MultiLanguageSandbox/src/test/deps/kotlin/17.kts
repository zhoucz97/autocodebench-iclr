
/**
 * Performs a right circular shift on a given unsigned 32-bit integer.
 * The function shifts the bits of the input number to the right by a specified number of positions.
 * The bits that are shifted out on the right are reinserted on the left side.
 * 
 * Example:
 * >>> circularRightShift(1023u, 20)
 *     4190208u
 * >>> circularRightShift(15u, 2)
 *     3221225475u
 * >>> circularRightShift(256u, 8)
 *     1u
 */
fun circularRightShift(num: UInt, shift: Int): UInt 
{
    // Ensure the shift is within the range of 0 to 31
    val effectiveShift = shift % 32
    
    // Perform the right shift
    val shiftedNum = num shr effectiveShift
    
    // Calculate the bits that were shifted out
    val shiftedOutBits = num shl (32 - effectiveShift) ushr 32
    
    // Combine the shifted number with the shifted-out bits
    return (shiftedNum or shiftedOutBits)
}

// Test cases
fun main() {
    check(circularRightShift(1023u, 20) == 4190208u)
    check(circularRightShift(15u, 2) == 3221225475u)
    check(circularRightShift(256u, 8) == 1u)
    check(circularRightShift(123u, 5) == 3623878659u)
    check(circularRightShift(4294967295u, 1) == 4294967295u)
    check(circularRightShift(1024u, 10) == 1u)
    check(circularRightShift(0u, 15) == 0u)
}
main()