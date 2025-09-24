object QuickSortProgram {
  def quicksort(list: List[Int]): List[Int] = {
    if (list == null || list.length <= 1)
      if (list == null) List() else list
    else {
      val pivot = list.head
      val rest = list.tail
      val less = rest.filter(_ < pivot)
      val greater = rest.filter(_ >= pivot)
      
      quicksort(less) ++ List(pivot) ++ quicksort(greater)
    }
  }
  
  def checkQuicksort(): Unit = {
    assert(quicksort(List(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)) == List(1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 9))
    assert(quicksort(List(5, 4, 3, 2, 1)) == List(1, 2, 3, 4, 5))
    assert(quicksort(List()) == List())
    assert(quicksort(List(1)) == List(1))
  }
  
  def main(args: Array[String]): Unit = {
    checkQuicksort()
  }
}
