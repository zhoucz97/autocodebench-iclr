import scala.collection.mutable.Queue

import scala.collection.mutable.{Queue, ListBuffer}

class TreeNode(var _value: Int) {
  var value: Int = _value
  var left: TreeNode = null
  var right: TreeNode = null
}

object Solution {
  /**
    * Return the level order traversal of a binary tree.
    *
    
    * @param root The root node of the binary tree.
    * @return The level order traversal as a list of lists, where each inner list represents a level of the tree.
    *
    * Example:
    * levelOrder(TreeNode(3, TreeNode(9), TreeNode(20, TreeNode(15), TreeNode(7))))
    * Output: List(List(3), List(9, 20), List(15, 7))
    */
  def levelOrder(root: TreeNode): List[List[Int]] = {
    if (root == null) return List.empty
    
    val result = scala.collection.mutable.ListBuffer[List[Int]]()
    val queue = Queue[TreeNode]()
    queue.enqueue(root)
    
    while (queue.nonEmpty) {
      val levelSize = queue.size
      val currentLevel = scala.collection.mutable.ListBuffer[Int]()
      
      for (_ <- 0 until levelSize) {
        val node = queue.dequeue()
        currentLevel += node.value
        
        if (node.left != null) queue.enqueue(node.left)
        if (node.right != null) queue.enqueue(node.right)
      }
      
      result += currentLevel.toList
    }
    
    result.toList
  }

  def main(args: Array[String]): Unit = {
    // Test cases
    val example1 = new TreeNode(1)
    example1.left = new TreeNode(2)
    example1.right = new TreeNode(3)

    assert(levelOrder(example1) == List(List(1), List(2, 3)))

    val example2 = new TreeNode(4)
    example2.left = new TreeNode(2)
    example2.right = new TreeNode(6)
    example2.left.left = new TreeNode(1)
    example2.left.right = new TreeNode(3)
    example2.right.left = new TreeNode(5)
    example2.right.right = new TreeNode(7)

    assert(levelOrder(example2) == List(List(4), List(2, 6), List(1, 3, 5, 7)))
    
    val example3 = new TreeNode(8)

    assert(levelOrder(example3) == List(List(8)))

    println("All tests passed")
  }
}