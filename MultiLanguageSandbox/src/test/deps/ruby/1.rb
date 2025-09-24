

# Check if in given list of numbers, any two numbers are closer to each other than
# given threshold.
#>>> has_close_elements([1.0, 2.0, 3.0], 0.5)
#False
#>>> has_close_elements([1.0, 2.8, 3.0, 4.0, 5.0, 2.0], 0.3)
#True
def has_close_elements(numbers, threshold)
  # Sort the numbers to easily  sorted_numbers = numbers.sort
  
  # Iterate through the sorted list and  (0...sorted_numbers.length - 1).each do |i|
    if (sorted_numbers[i+1] - sorted_numbers[i]) < threshold
      return true
    end
  end
  
  false
end

  # Test cases
  def check(has_close_elements)
    raise 'Test failed' unless has_close_elements.call([1.0, 2.0, 3.9, 4.0, 5.0, 2.2], 0.3)
    raise 'Test failed' if has_close_elements.call([1.0, 2.0, 3.9, 4.0, 5.0, 2.2], 0.05)
    raise 'Test failed' unless has_close_elements.call([1.0, 2.0, 5.9, 4.0, 5.0], 0.95)
    raise 'Test failed' if has_close_elements.call([1.0, 2.0, 5.9, 4.0, 5.0], 0.8)
    raise 'Test failed' unless has_close_elements.call([1.0, 2.0, 3.0, 4.0, 5.0, 2.0], 0.1)
    raise 'Test failed' unless has_close_elements.call([1.1, 2.2, 3.1, 4.1, 5.1], 1.0)
    raise 'Test failed' if has_close_elements.call([1.1, 2.2, 3.1, 4.1, 5.1], 0.5)
  
    puts 'All tests passed!'
  end
  
  check(method(:has_close_elements))