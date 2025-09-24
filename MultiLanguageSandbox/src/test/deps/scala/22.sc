

class TreeNode(val value: Int, var left: TreeNode = null, var right: TreeNode = null)

object Solution {
  /**
    * Check if a binary tree is symmetric.
    *
    * @param root The root node of the binary tree.
    * @return True if the binary tree is symmetric, False otherwise.
    *
    * Example:
    * isSymmetric(TreeNode(1, TreeNode(2, TreeNode(3), TreeNode(4)), TreeNode(2, TreeNode(4), TreeNode(3))))
    * Output: true
    */
  def isSymmetric(root: TreeNode): Boolean = {
    if (root == null) return true
    isMirror(root.left, root.right)
  }

  private def isMirror(left: TreeNode, right: TreeNode): Boolean = {
    if (left == null && right == null) return true
    if (left == null || right == null) return false
    left.value == right.value && isMirror(left.left, right.right) && isMirror(left.right, right.left)
  }

def main(args: Array[String]): Unit = {
    // Test Case 1
    val root1 = new TreeNode(1,
      new TreeNode(2,
        new TreeNode(3), new TreeNode(4)
      ),
      new TreeNode(2,
        new TreeNode(4), new TreeNode(3)
      )
    )
    assert(isSymmetric(root1))

    // Test Case 2
    val root2 = new TreeNode(1,
      new TreeNode(2,
        null, new TreeNode(3)
      ),
      new TreeNode(2,
        null, new TreeNode(3)
      )
    )
    assert(!isSymmetric(root2))

    // Test Case 3
    val root3 = new TreeNode(1,
      new TreeNode(2,
        new TreeNode(3), null
      ),
      new TreeNode(2,
        new TreeNode(3), null
      )
    )
    assert(!isSymmetric(root3))

    println("All tests passed")
  }
}