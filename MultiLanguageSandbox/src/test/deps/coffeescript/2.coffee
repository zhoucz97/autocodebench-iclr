

class ListNode
  constructor: (@val = null, @next = null) ->

# Given the head of a singly linked list, reverse the list and return the reversed list.
# @param head The head node of the linked list.
# @return The head node of the reversed linked list.
#
# Example 1:
# Input: head = [1,2,3,4,5]
# Output: [5,4,3,2,1]
#
# Example 2:
# Input: head = [1,2]
# Output: [2,1]
#
# Example 3:
# Input: head = []
# Output: []
#
# Definition for singly-linked list:
# class ListNode
#     constructor: (@val = null, @next = null) ->
reverseList = (head) ->
  prev = null
  current = head
  
  while current
    next = current.next  # Store the next node
    current.next = prev  # Reverse the current node's pointer
    prev = current       # Move prev to current
    current = next       # Move current to next node
  
  prev  # prev is now the new head
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected

main = ->
  # Test case 1
  head1 = new ListNode(1)
  head1.next = new ListNode(2)
  head1.next.next = new ListNode(3)
  head1.next.next.next = new ListNode(4)
  head1.next.next.next.next = new ListNode(5)

  reversedHead1 = reverseList(head1)
  assertEqual reversedHead1.val, 5
  assertEqual reversedHead1.next.val, 4
  assertEqual reversedHead1.next.next.val, 3
  assertEqual reversedHead1.next.next.next.val, 2
  assertEqual reversedHead1.next.next.next.next.val, 1

  # Test case 2
  head2 = new ListNode(1)
  head2.next = new ListNode(2)

  reversedHead2 = reverseList(head2)
  assertEqual reversedHead2.val, 2
  assertEqual reversedHead2.next.val, 1

  # Test case 3
  head3 = null

  reversedHead3 = reverseList(head3)
  assertEqual reversedHead3, null

  console.log("All tests passed")

main()