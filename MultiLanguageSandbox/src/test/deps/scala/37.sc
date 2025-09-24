

object Solution {
  case class ListNode(var value: Int, var next: ListNode = null)
  /**
    * Remove the nth node from the end of a linked list and return the head of the modified list.
    *
    * @param head The head of the linked list.
    * @param n The position of the node to be removed from the end of the list.
    * @return The head of the modified list.
    *
    * Example:
    * removeNthFromEnd(ListNode(1, ListNode(2, ListNode(3, ListNode(4, ListNode(5))))), 2)
    * Output: ListNode(1, ListNode(2, ListNode(3, ListNode(5))))
    */
  def removeNthFromEnd(head: ListNode, n: Int): ListNode = {
    val dummy = new ListNode(0)
    dummy.next = head
    var fast = dummy
    var slow = dummy
    
    // Move fast n steps ahead
    for (_ <- 0 until n) {
      fast = fast.next
    }
    
    // Move both pointers until fast reaches the end
    while (fast.next != null) {
      fast = fast.next
      slow = slow.next
    }
    
    // Remove the nth node from the end
    slow.next = slow.next.next
    
    dummy.next
  }

  def main(args: Array[String]): Unit = {
  // Test cases
  val test1 = createList(Array(1, 2, 3, 4, 5))
  assert(listToString(removeNthFromEnd(test1, 2)) == "1->2->3->5")

  val test2 = createList(Array(1))
  assert(listToString(removeNthFromEnd(test2, 1)) == "")

  val test3 = createList(Array(1, 2))
  assert(listToString(removeNthFromEnd(test3, 1)) == "1")

  println("All tests passed")}
}