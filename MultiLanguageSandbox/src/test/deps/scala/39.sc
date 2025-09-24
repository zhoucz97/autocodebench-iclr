

import scala.annotation.tailrec

object Solution {
    class ListNode(var x: Int) {
    var next: ListNode = null
  }
    /**
      * Swap every two adjacent nodes in a linked list and return its head.
      *
      * @param head The head node of the linked list.
      * @return The head node of the linked list after swapping adjacent nodes.
      *
      * Example:
      * swapPairs(ListNode(1, ListNode(2, ListNode(3, ListNode(4))))) should return ListNode(2, ListNode(1, ListNode(4, ListNode(3))))
      * swapPairs(null) should return null
      * swapPairs(ListNode(1)) should return ListNode(1)
      */
  def swapPairs(head: ListNode): ListNode = {
    if (head == null || head.next == null) {
      return head
    }
    
    val newHead = head.next
    head.next = swapPairs(newHead.next)
    newHead.next = head
    
    newHead
  }

    def main(args: Array[String]): Unit = {
    // Test cases
    val test1 = createList(Array(5, 6, 7, 8))
    assert(listToString(swapPairs(test1)) == "6->5->8->7")

    val test2 = createList(Array())
    assert(listToString(swapPairs(test2)) == "")

    val test3 = createList(Array(9))
    assert(listToString(swapPairs(test3)) == "9")

    println("All tests passed")
  }
}