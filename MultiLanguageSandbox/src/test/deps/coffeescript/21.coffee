

# Given the root of a binary tree, return the level order traversal of its nodes from bottom to top.
# @param root The root node of the binary tree.
# @return The level order traversal of the binary tree from bottom to top.
#
# Example 1:
# Input: root = [3,9,20,null,null,15,7]
# Output: [[15,7],[9,20],[3]]
#
# Example 2:
# Input: root = [1]
# Output: [[1]]
#
# Example 3:
# Input: root = []
# Output: []
levelOrderBottom = (root) ->
  return [] unless root

  result = []
  queue = [root]

  while queue.length > 0
    levelSize = queue.length
    currentLevel = []

    for i in [0...levelSize]
      node = queue.shift()
      currentLevel.push(node.val)
      queue.push(node.left) if node.left
      queue.push(node.right) if node.right

    result.unshift(currentLevel)

  result
class AssertionError extends Error

assertEqual = (actual, expected, message = null) ->
  throw new AssertionError(message) unless actual is expected
# Test Cases
main = ->
  root1 =
    val: 3
    left:
      val: 9
      left: null
      right: null
    right:
      val: 20
      left:
        val: 15
        left: null
        right: null
      right:
        val: 7
        left: null
        right: null

  result1 = levelOrderBottom(root1)
  assertEqual(result1.length, 3)
  assertEqual(result1[0].length, 2)
  assertEqual(result1[0][0], 15)
  assertEqual(result1[0][1], 7)
  assertEqual(result1[1].length, 2)
  assertEqual(result1[1][0], 9)
  assertEqual(result1[1][1], 20)
  assertEqual(result1[2].length, 1)
  assertEqual(result1[2][0], 3)

  root2 =
    val: 1
    left: null
    right: null

  result2 = levelOrderBottom(root2)
  assertEqual(result2.length, 1)
  assertEqual(result2[0].length, 1)
  assertEqual(result2[0][0], 1)

  root3 = null
  result3 = levelOrderBottom(root3)
  assertEqual(result3.length, 0)

  console.log("All tests passed")

main()