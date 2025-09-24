import scala.collection.mutable.Queue
import scala.collection.mutable.{Queue, Set}

import scala.collection.mutable.ListBuffer
object Solution {
  /**
    * Remove the minimum number of invalid parentheses from the given string to make it valid.
    *
    * @param s The input string containing parentheses and letters.
    * @return A list of all possible valid strings after removing invalid parentheses.
    *
    * Example:
    * removeInvalidParentheses("()())()")
    * Output: List("(())()", "()()()")
    */
  def removeInvalidParentheses(s: String): List[String] = {
        val result = scala.collection.mutable.ListBuffer[String]()
        val visited = scala.collection.mutable.Set[String]()
        val queue = Queue[String]()
        var found = false
        
        queue.enqueue(s)
        visited.add(s)
        
        while (queue.nonEmpty) {
            val current = queue.dequeue()
            
            if (isValid(current)) {
                result += current
                found = true
            }
            
            if (found) {
                // No need to process further levels once we've found valid strings at this level
                continue
            } else {
                // Generate all possible strings by removing one parenthesis
                for (i <- 0 until current.length) {
                    if (current(i) != '(' && current(i) != ')') {
                        continue
                    }
                    val next = current.substring(0, i) + current.substring(i + 1)
                    if (!visited.contains(next)) {
                        visited.add(next)
                        queue.enqueue(next)
                    }
                }
            }
        }
        
        if (result.isEmpty) {
            List("")
        } else {
            result.toList.distinct
        }
    }
    
    private def isValid(s: String): Boolean = {
        var balance = 0
        for (c <- s) {
            if (c == '(') {
                balance += 1
            } else if (c == ')') {
                balance -= 1
                if (balance < 0) {
                    return false
                }
            }
        }
        balance == 0
    }

  def main(args: Array[String]): Unit = {
    // Test 1
    val result1 = removeInvalidParentheses("()())()")
    assert(result1.contains("(())()"))
    assert(result1.contains("()()()"))

    // Test 2
    val result2 = removeInvalidParentheses("(a)())()")
    assert(result2.contains("(a())()"))
    assert(result2.contains("(a)()()"))

    // Test 3
    val result3 = removeInvalidParentheses(")(")
    assert(result3.contains(""))

    println("All tests passed")
  }
}