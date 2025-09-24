

case class TreeNode(var value: Int, var left: Option[TreeNode] = None, var right: Option[TreeNode] = None)
object Solution {

    /**
     * Given the root of a binary search tree, returns the minimum difference between any two different nodes values in the tree.
     *
     * @param root The root node of the binary search tree.
     * @return The minimum difference between any two different nodes values.
     *
     * Example:
     * minDiffInBST(TreeNode(4, TreeNode(2, TreeNode(1), TreeNode(3)), TreeNode(6)))
     * Output: 1
     */
  def minDiffInBST(root: TreeNode): Int = {
    var prev: Int = -1
    var minDiff: Int = Int.MaxValue
    
    def inorder(node: TreeNode): Unit = {
      if (node == null) return
      inorder(node.left)
      if (prev != -1) {
        minDiff = math.min(minDiff, node.value - prev)
      }
      prev = node.value
      inorder(node.right)
    }
    
    inorder(root)
    minDiff
  }

  def main(args: Array[String]): Unit = {
    val root = TreeNode(5, Some(TreeNode(3)), Some(TreeNode(7)))
    root.left.get.left = Some(TreeNode(2))
    root.left.get.right = Some(TreeNode(4))
    root.right.get.left = Some(TreeNode(6))
    root.right.get.right = Some(TreeNode(8))

    assert(minDiffInBST(root) == 1)

    val root2 = TreeNode(8, Some(TreeNode(4)), Some(TreeNode(12)))
    root2.left.get.left = Some(TreeNode(2))
    root2.left.get.right = Some(TreeNode(6))
    root2.right.get.left = Some(TreeNode(10))
    root2.right.get.right = Some(TreeNode(14))

    assert(minDiffInBST(root2) == 2)

    val root3 = TreeNode(1)
    root3.right = Some(TreeNode(3))
    root3.right.get.right = Some(TreeNode(6))
    root3.right.get.right.get.left = Some(TreeNode(4))

    assert(minDiffInBST(root3) == 1)

    println("All tests passed")
  }
}