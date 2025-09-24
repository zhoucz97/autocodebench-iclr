

// Definition for a binary tree node.
case class TreeNode(value: Int, left: TreeNode = null, right: TreeNode = null)

object Solution {
  /**
    * Determine whether a binary tree is a valid binary search tree.
    *
    * @param root The root node of the binary tree.
    * @return True if the binary tree is a valid binary search tree, false otherwise.
    *
    * Example:
    * isValidBST(TreeNode(2, TreeNode(1), TreeNode(3))) -> true
    * isValidBST(TreeNode(5, TreeNode(1), TreeNode(4, TreeNode(3), TreeNode(6)))) -> false
    */
  def isValidBST(root: TreeNode): Boolean = {
    def helper(node: TreeNode, lower: Long, upper: Long): Boolean = {
      if (node == null) true
      else {
        val value = node.value.toLong
        (value > lower && value < upper) &&
        helper(node.left, lower, value) &&
        helper(node.right, value, upper)
      }
    }
    helper(root, Long.MinValue, Long.MaxValue)
  }

  def main(args: Array[String]): Unit = {
    // Test Case 1
    val root1 = new TreeNode(2, new TreeNode(1), new TreeNode(3))
    assert(isValidBST(root1), "Test Case 1 Failed")

    // Test Case 2
    val root2 = new TreeNode(5, new TreeNode(1), new TreeNode(4, new TreeNode(3), new TreeNode(6)))
    assert(!isValidBST(root2), "Test Case 2 Failed")

    // Test Case 3
    val root3 = new TreeNode(4, new TreeNode(2, new TreeNode(1), new TreeNode(3)), new TreeNode(5))
    assert(isValidBST(root3), "Test Case 3 Failed")

    println("All tests passed")
  }
}