

class TreeNode(var value: Int, var left: TreeNode = null, var right: TreeNode = null)
object Solution {
  /**
    * Determine if a given binary tree is a univalued tree.
    *
    * @param root The root node of the binary tree.
    * @return True if the binary tree is a univalued tree, False otherwise.
    *
    * Example:
    * isUnivalTree(TreeNode(1, TreeNode(1, TreeNode(1), TreeNode(1)), TreeNode(1, null, TreeNode(1))))
    * Output: true
    *
    * isUnivalTree(TreeNode(2, TreeNode(2, TreeNode(2, TreeNode(5), TreeNode(2))), null))
    * Output: false
    */
  def isUnivalTree(root: TreeNode): Boolean = {
    if (root == null) return true
    
    val rootValue = root.value
    
    def helper(node: TreeNode): Boolean = {
      if (node == null) true
      else if (node.value != rootValue) false
      else helper(node.left) && helper(node.right)
    }
    
    helper(root)
  }

  def main(args: Array[String]): Unit = {
    // Test Case 1
    val tree1 = new TreeNode(2)
    tree1.left = new TreeNode(2)
    tree1.left.left = new TreeNode(2)
    tree1.left.left.left = new TreeNode(5)
    tree1.left.left.right = new TreeNode(2)
    assert(!isUnivalTree(tree1))  // Output: false

    // Test Case 2
    val tree2 = new TreeNode(1)
    tree2.left = new TreeNode(1)
    tree2.left.left = new TreeNode(1)
    tree2.left.right = new TreeNode(1)
    tree2.right = new TreeNode(1)
    tree2.right.right = new TreeNode(1)
    assert(isUnivalTree(tree2))  // Output: true

    // Test Case 3
    val tree3 = new TreeNode(3)
    tree3.left = new TreeNode(3)
    tree3.right = new TreeNode(3)
    assert(isUnivalTree(tree3))  // Output: true

    println("All tests passed")
  }
}
