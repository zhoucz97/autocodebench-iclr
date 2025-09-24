

class TreeNode
  constructor: (val) ->
    @val = val
    @left = null
    @right = null

# Given the root of a binary tree, return the zigzag level order traversal of its nodes' values.
# The zigzag level order traversal is a traversal of the nodes' values in a zigzag pattern (i.e., from left to right, then right to left for the next level and alternate between).
#
# @param root The root of the binary tree.
# @return The zigzag level order traversal of the nodes' values.
#
# Example 1:
# Input: root = [3,9,20,null,null,15,7]
# Output: [[3],[20,9],[15,7]]
#
# Example 2:
# Input: root = [1]
# Output: [[1]]
#
# Example 3:
# Input: root = []
# Output: []
#
# Constraints:
# - The number of nodes in the tree is in the range [0, 2000].
# - -100 <= Node.val <= 100
zigzagLevelOrder = (root) ->
  return [] unless root

  result = []
  queue = [root]
  leftToRight = true

  while queue.length > 0
    levelSize = queue.length
    currentLevel = []

    for i in [0...levelSize]
      node = queue.shift()
      currentLevel.push(node.val)
      queue.push(node.left) if node.left
      queue.push(node.right) if node.right

    if not leftToRight
      currentLevel.reverse()

    result.push(currentLevel)
    leftToRight = !leftToRight

  result
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

  result1 = zigzagLevelOrder(root1)
  assertEqual result1.length, 3, "Test Case 1"
  assertEqual result1[0].toString(), [3].toString(), "Test Case 1"
  assertEqual result1[1].toString(), [20, 9].toString(), "Test Case 1"
  assertEqual result1[2].toString(), [15, 7].toString(), "Test Case 1"

  root2 = new TreeNode(1)
  result2 = zigzagLevelOrder(root2)
  assertEqual result2.length, 1, "Test Case 2"
  assertEqual result2[0].toString(), [1].toString(), "Test Case 2"

  root3 = null
  result3 = zigzagLevelOrder(root3)
  assertEqual result3.length, 0, "Test Case 3"

  console.log "All tests passed"

main()