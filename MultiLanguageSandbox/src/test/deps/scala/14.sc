

object Solution {
  case class TreeNode(value: Int, left: TreeNode = null, right: TreeNode = null)

  /**
    * Find the maximum depth of a binary tree.
    *
    * @param root The root node of the binary tree.
    * @return The maximum depth of the binary tree.
    *
    * Example:
    * maxDepth(TreeNode(3, TreeNode(9), TreeNode(20, TreeNode(15), TreeNode(7))))
    * Output: 3
    */
  def maxDepth(root: TreeNode): Int = {
    if (root == null) 0
    else 1 + math.max(maxDepth(root.left), maxDepth(root.right))
  }

  def main(args: Array[String]): Unit = {
    val root1 = TreeNode(3, TreeNode(9), TreeNode(20, TreeNode(15), TreeNode(7)))
    val root2 = TreeNode(1, TreeNode(2), TreeNode(3, TreeNode(4), TreeNode(5, TreeNode(6), null)))
    val root3 = TreeNode(10, null, null)

    assert(maxDepth(root1) == 3)
    assert(maxDepth(root2) == 4)
    assert(maxDepth(root3) == 1)

    println("All tests passed")
  }
}