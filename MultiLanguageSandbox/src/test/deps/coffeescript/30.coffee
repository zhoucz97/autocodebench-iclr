

class TreeNode
  constructor: (val) ->
    @val = val
    @left = null
    @right = null

# Given a binary tree, determine if it is height-balanced.
# A height-balanced binary tree is defined as a binary tree in which the
# depth of the two subtrees of every node never differ by more than 1.
#
# @param root The root node of the binary tree.
# @return True if the binary tree is height-balanced, False otherwise.
#
# Example 1:
# Input: root = [3,9,20,null,null,15,7]
# Output: true
#
# Example 2:
# Input: root = [1,2,2,3,3,null,null,4,4]
# Output: false
#
# Example 3:
# Input: root = []
# Output: true
isBalanced = (root) ->
  # Helper function to check balance and get height
  checkBalance = (node) ->
    return [true, 0] unless node?  # Base case: empty tree is balanced with height 0

    [leftBalanced, leftHeight] = checkBalance(node.left)
    [rightBalanced, rightHeight] = checkBalance(node.right)

    # Check if both subtrees are balanced and their heights differ by at most 1
    currentBalanced = leftBalanced && rightBalanced && Math.abs(leftHeight - rightHeight) <= 1
    currentHeight = Math.max(leftHeight, rightHeight) + 1

    [currentBalanced, currentHeight]

  [balanced, _] = checkBalance(root)
  balanced
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
# Test Cases
main = ->
  root1 = new TreeNode(3)
  root1.left = new TreeNode(9)
  root1.right = new TreeNode(20)
  root1.right.left = new TreeNode(15)
  root1.right.right = new TreeNode(7)
  assertEqual isBalanced(root1), true, 'Test Case 1'

  root2 = new TreeNode(1)
  root2.left = new TreeNode(2)
  root2.right = new TreeNode(2)
  root2.left.left = new TreeNode(3)
  root2.left.right = new TreeNode(3)
  root2.left.left.left = new TreeNode(4)
  root2.left.left.right = new TreeNode(4)
  assertEqual isBalanced(root2), false, 'Test Case 2'

  root3 = null
  assertEqual isBalanced(root3), true, 'Test Case 3'

  console.log 'All tests passed'

main()