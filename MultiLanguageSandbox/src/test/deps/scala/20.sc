import scala.collection.mutable.Queue

class TreeNode(var _value: Int = 0) {
  var value: Int = _value
  var left: TreeNode = null
  var right: TreeNode = null
}
object Solution {
  /**
   * Calculate the minimum depth of a binary tree.
   *
   * The minimum depth is the number of nodes along the shortest path from the root node to
   * the nearest leaf node.
   *
   * @param root The root node of the binary tree.
   * @return The minimum depth of the tree.
   *
   * Examples:
   * minimumDepth([3,9,20,null,null,15,7])
   * Output: 2
   *
   * minimumDepth([2,null,3,null,4,null,5,null,6])
   * Output: 5
   *
   * Constraints:
   * - The number of nodes in the tree is in the range [0, 10^5].
   * - The value of each node is in the range [-1000, 1000].
   */

  def minimumDepth(root: TreeNode): Int = {
    if (root == null) return 0
    
    val queue = Queue[(TreeNode, Int)]()
    queue.enqueue((root, 1))
    
    while (queue.nonEmpty) {
      val (node, depth) = queue.dequeue()
      
      if (node.left == null && node.right == null) {
        return depth
      }
      
      if (node.left != null) {
        queue.enqueue((node.left, depth + 1))
      }
      
      if (node.right != null) {
        queue.enqueue((node.right, depth + 1))
      }
    }
    
    0 // This line is theoretically unreachable for non-null root, but required for compilation
  }

  def main(args: Array[String]): Unit = {
    // Test case 1
    val root1 = new TreeNode(3)
    root1.left = new TreeNode(9)
    root1.right = new TreeNode(20)
    root1.right.left = new TreeNode(15)
    root1.right.right = new TreeNode(7)
    assert(minimumDepth(root1) == 2)
    
    // Test case 2
    val root2 = new TreeNode(2)
    root2.right = new TreeNode(3)
    root2.right.right = new TreeNode(4)
    root2.right.right.right = new TreeNode(5)
    root2.right.right.right.right = new TreeNode(6)
    assert(minimumDepth(root2) == 5)
    
    // Test case 3
    val root3: TreeNode = null
    assert(minimumDepth(root3) == 0)
    
    println("All tests passed")
  }
}