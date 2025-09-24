import scala.collection.mutable.Queue

class TreeNode(var _value: Int = 0) {
  var value: Int = _value
  var left: TreeNode = null
  var right: TreeNode = null
}

object Solution {
    /**
      * Find the value of the bottom-most left node in a binary tree.
      *
      * @param root The root node of the binary tree.
      * @return The value of the bottom-most left node.
      *
      * Example:
      * findBottomLeftValue(new TreeNode(2, new TreeNode(1), new TreeNode(3)))
      * Output: 1
      *
      * findBottomLeftValue(new TreeNode(1, new TreeNode(2, new TreeNode(4), null), new TreeNode(3, new TreeNode(5, new TreeNode(7), new TreeNode(6))), null))
      * Output: 7
      */
  def findBottomLeftValue(root: TreeNode): Int = {
    if (root == null) return -1 // assuming null root is handled, though problem says root is given
    
    val queue = Queue[TreeNode]()
    queue.enqueue(root)
    var leftmostValue = root.value
    
    while (queue.nonEmpty) {
      val levelSize = queue.size
      for (i <- 0 until levelSize) {
        val currentNode = queue.dequeue()
        if (i == 0) {
          leftmostValue = currentNode.value
        }
        if (currentNode.left != null) {
          queue.enqueue(currentNode.left)
        }
        if (currentNode.right != null) {
          queue.enqueue(currentNode.right)
        }
      }
    }
    
    leftmostValue
  }

  def main(args: Array[String]): Unit = {
    // Test Case 1
    val root1 = new TreeNode(2)
    root1.left = new TreeNode(1)
    root1.right = new TreeNode(3)
    assert(findBottomLeftValue(root1) == 1)

    // Test Case 2
    val root2 = new TreeNode(1)
    root2.left = new TreeNode(2)
    root2.left.left = new TreeNode(4)
    root2.right = new TreeNode(3)
    root2.right.left = new TreeNode(5)
    root2.right.left.left = new TreeNode(7)
    root2.right.left.right = new TreeNode(6)
    assert(findBottomLeftValue(root2) == 7)

    // Test Case 3
    val root3 = new TreeNode(5)
    root3.left = new TreeNode(3)
    root3.right = new TreeNode(6)
    root3.left.left = new TreeNode(2)
    root3.left.right = new TreeNode(4)
    root3.left.left.left = new TreeNode(1)
    assert(findBottomLeftValue(root3) == 1)

    println("All tests passed")
  }
}