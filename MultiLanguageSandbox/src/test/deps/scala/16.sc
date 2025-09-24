

object Solution {

  case class TreeNode(value: Int, left: TreeNode = null, right: TreeNode = null)

   /**
    * Find the diameter of a binary tree.
    *
    * @param root The root node of the binary tree.
    * @return The length of the longest path between any two nodes in the tree.
    *
    * Example:
    * diameterOfBinaryTree(TreeNode(1, TreeNode(2), TreeNode(3, TreeNode(4), TreeNode(5))))
    * Output: 3
    *
    * Note:
    * The diameter of a binary tree is defined as the length of the longest path between any two nodes,
    * which may or may not pass through the root.
    * The length of a path is represented by the number of edges between two nodes.
    * The given tree is guaranteed to have at most 104 nodes,
    * and the values of the nodes in the tree are in the range [-100, 100].
    */
  def diameterOfBinaryTree(root: TreeNode): Int = {
    var diameter = 0

    def height(node: TreeNode): Int = {
      if (node == null) return 0
      val leftHeight = height(node.left)
      val rightHeight = height(node.right)
      // Update the diameter if the current path (left + right) is larger
      diameter = math.max(diameter, leftHeight + rightHeight)
      // Return the height of the current node
      1 + math.max(leftHeight, rightHeight)
    }

    height(root)
    diameter
  }

  def main(args: Array[String]): Unit = {
    // Test case 1
    val root1 = TreeNode(1, TreeNode(2), TreeNode(3, TreeNode(4), TreeNode(5)))
    assert(diameterOfBinaryTree(root1) == 3)

    // Test case 2
    val root2 = TreeNode(1, TreeNode(2, TreeNode(3, TreeNode(4))))
    assert(diameterOfBinaryTree(root2) == 3)

    // Test case 3
    val root3 = TreeNode(1, null, TreeNode(2, null, TreeNode(3, null, TreeNode(4))))
    assert(diameterOfBinaryTree(root3) == 3)

    println("All tests passed")
  }
}