

# Finds the intersection point of two linear functions.
# This function takes four arguments: slope1, intercept1, slope2, intercept2.
# slope1 and slope2 are the slopes of the two lines, while intercept1 and intercept2 are their y-intercepts.
# The function calculates and returns the coordinates of the intersection point of these two lines, if it exists.
# The coordinates are returned as a two-element array [x, y], rounded to two decimal places.
#
# Example:
# >>> find_intersection(1.0, 0.0, -1.0, 2.0)
# [1.00, 1.00]
# >>> find_intersection(2.0, 3.0, 2.0, -1.0)
# nil # Parallel lines with no intersection


def find_intersection(slope1, intercept1, slope2, intercept2)
  # Check if the lines are parallel (same slope)
  if slope1 == slope2
    return nil  # No intersection or infinite intersections (same line)
  end
  
  # Calculate x-coordinate of intersection
  x = (intercept2 - intercept1) / (slope1 - slope2)
  
  # Calculate y-coordinate using either line equation
  y = slope1 * x + intercept1
  
  # Round to two decimal places and return as array
  [x.round(2), y.round(2)]
end

raise 'Test failed' unless find_intersection(1.0, 0.0, -1.0, 2.0) == [1.00, 1.00]
raise 'Test failed' unless find_intersection(2.0, 3.0, 2.0, -1.0).nil?
raise 'Test failed' unless find_intersection(0.5, 1.0, -0.5, -1.0) == [-2.00, 0.00]
raise 'Test failed' unless find_intersection(3.0, 2.0, -1.0, 4.0) == [0.50, 3.50]
raise 'Test failed' unless find_intersection(2.0, 0.0, 0.0, 2.0) == [1.0, 2.0]
  

puts 'All tests passed!'