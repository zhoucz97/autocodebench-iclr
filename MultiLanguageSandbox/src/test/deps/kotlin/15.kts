
import java.util.LinkedList

/**
    * Finds the last remaining number in a sequence of n numbers, where numbers are eliminated in a cycle of every third element.
    * 
    * The process begins with the first element in a sequence from 1 to n, counting up to three. Each time the count reaches three,
    * the current number is removed from the sequence. This process continues until only one number remains in the sequence.
    *
    * Examples:
    * >>> findLastRemaining(5)
        4
    * >>> findLastRemaining(10)
        4
**/


fun findLastRemaining(n: Int): Int 
{
    if (n == 1) return 1
    var list = mutableListOf<Int>()
    for (i in 1..n) {
        list.add(i)
    }
    var index = 0
    while (list.size > 1) {
        index = (index + 2) % list.size
        list.removeAt(index)
    }
    return list[0]
}
fun main(){
    check(findLastRemaining(5) == 4)
    check(findLastRemaining(10) == 4)
    check(findLastRemaining(6) == 1)
    check(findLastRemaining(7) == 4)
    check(findLastRemaining(15) == 5)
    check(findLastRemaining(20) == 20)
    check(findLastRemaining(1) == 1)
}
main()