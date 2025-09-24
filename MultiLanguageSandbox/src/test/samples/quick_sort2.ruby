require 'minitest/autorun'

def quicksort(list)
  return [] if list.nil?
  return list if list.length <= 1

  pivot = list[0]
  rest = list[1..-1]
  less = rest.select { |x| x < pivot }
  greater = rest.select { |x| x >= pivot }

  quicksort(less) + [pivot] + quicksort(greater)
end

class TestQuicksort < Minitest::Test
  def test_mixed_numbers_with_duplicates
    assert_equal [1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 9], 
                 quicksort([3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5])
  end

  def test_reverse_sorted_list
    assert_equal [1, 2, 3, 4, 5], 
                 quicksort([5, 4, 3, 2, 1])
  end

  def test_empty_list
    assert_equal [], quicksort([])
  end

  def test_single_element
    assert_equal [1], quicksort([1])
  end

  def test_nil_input
    assert_equal [], quicksort(nil)
  end
end
