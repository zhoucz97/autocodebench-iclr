def quicksort(list)
  return [] if list.nil?
  return list if list.length <= 1
  
  pivot = list[0]
  rest = list[1..-1]
  less = rest.select { |x| x < pivot }
  greater = rest.select { |x| x >= pivot }
  
  quicksort(less) + [pivot] + quicksort(greater)
end

def check_quicksort
  raise unless quicksort([3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]) == [1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 9]
  raise unless quicksort([5, 4, 3, 2, 1]) == [1, 2, 3, 4, 5]
  raise unless quicksort([]) == []
  raise unless quicksort([1]) == [1]
end

check_quicksort
