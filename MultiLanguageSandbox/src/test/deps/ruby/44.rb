

# Compares the area of a square with side length 'a' to the area of a rectangle with sides 'b' and 'c'.
# The function takes three arguments, a, b, and c, which are positive integers representing the side length of the square and the sides of the rectangle, respectively.
# It returns a string indicating whose shape has the greater area: "Alice" if the square's area is larger, and "Bob" if the rectangle's area is larger.
#
# Examples:
# >>> compare_area(5, 4, 6)
# "Alice"
# >>> compare_area(7, 5, 10)
# "Bob"

def compare_area(a, b, c)
  square_area = a * a
  rectangle_area = b * c

  if square_area > rectangle_area
    "Alice"
  else
    "Bob"
  end
end

raise 'Test failed' unless compare_area(5, 4, 6) == "Alice"
raise 'Test failed' unless compare_area(7, 5, 10) == "Bob"
raise 'Test failed' unless compare_area(6, 3, 12) == "Bob"
raise 'Test failed' unless compare_area(10, 10, 1) == "Alice"
raise 'Test failed' unless compare_area(8, 8, 8) == "Bob" # Case where areas are equal, but based on the problem statement, Bob wins in a tie.