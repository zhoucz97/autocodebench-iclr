

class TreeNode
  constructor: (val) ->
    @val = val
    @left = null
    @right = null

# Given a binary search tree root and a target sum k, return true if there exist two elements in the binary search tree such that their sum is equal to the given target sum.
# @param root The root node of the binary search tree.
# @param k The target sum.
# @return true if there exist two elements in the binary search tree such that their sum is equal to the given target sum, false otherwise.
#
# Example 1:
# Input: root = [5,3,6,2,4,null,7], k = 9
# Output: true
#
# Example 2:
# Input: root = [5,3,6,2,4,null,7], k = 28
# Output: false
#
# Constraints:
# - The number of nodes in the binary search tree is in the range [1, 104].
# - Node.val is within the range [-104, 104].
# - The input root is a valid binary search tree.
# - The input k is within the range [-105, 105].
findTarget = (root, k) ->
  # Helper function to perform in-order traversal and collect values
  inOrderTraversal = (node, values) ->
    return unless node?
    inOrderTraversal(node.left, values)
    values.push(node.val)
    inOrderTraversal(node.right, values)

  values = []
  inOrderTraversal(root, values)

  # Two-pointer approach to find if any two values sum to k
  left = 0
  right = values.length - 1
  while left < right
    currentSum = values[left] + values[right]
    if currentSum == k
      return true
    else if currentSum < k
      left++
    else
      right--
  false
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
main = ->
  # Test case 1
  root1 = new TreeNode(5)
  root1.left = new TreeNode(3)
  root1.right = new TreeNode(6)
  root1.left.left = new TreeNode(2)
  root1.left.right = new TreeNode(4)
  root1.right.right = new TreeNode(7)

  assertEqual(findTarget(root1, 9), true, "Test case 1 failed")

  # Test case 2
  root2 = new TreeNode(5)
  root2.left = new TreeNode(3)
  root2.right = new TreeNode(6)
  root2.left.left = new TreeNode(2)
  root2.left.right = new TreeNode(4)
  root2.right.right = new TreeNode(7)

  assertEqual(findTarget(root2, 28), false, "Test case 2 failed")

  # Test case 3
  root3 = new TreeNode(1)
  root3.left = new TreeNode(2)
  root3.right = new TreeNode(3)
  root3.left.left = new TreeNode(4)
  root3.left.right = new TreeNode(5)
  root3.right.left = new TreeNode(6)
  root3.right.right = new TreeNode(7)

  assertEqual(findTarget(root3, 8), true, "Test case 3 failed")

  console.log("All tests passed")

main()