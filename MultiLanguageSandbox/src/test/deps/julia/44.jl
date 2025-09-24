function compare_area(a::Int, b::Int, c::Int) :: String
    """
    Determine whose geometric shape has a greater area, Alice with her square of side length `a`, 
    or Bob with his rectangle of dimensions `b` x `c`. Returns "Alice" if the square's area is larger, 
    and "Bob" if the rectangle's area is larger.

    # Examples
    - `compare_area(5, 4, 6)` returns "Alice" because the area of the square (25) is greater than the area of the rectangle (24).
    - `compare_area(7, 5, 10)` returns "Bob" because the area of the rectangle (50) is greater than the area of the square (49).
    """
    square_area = a * a
    rectangle_area = b * c
    
    if square_area > rectangle_area
        return "Alice"
    else
        return "Bob"
    end
end
@assert compare_area(5, 4, 6) == "Alice"
@assert compare_area(7, 5, 10) == "Bob"
@assert compare_area(2, 3, 1) == "Alice"  # Testing with a smaller square and rectangle, square's area is 4, rectangle's area is 3
@assert compare_area(8, 4, 4) == "Alice"