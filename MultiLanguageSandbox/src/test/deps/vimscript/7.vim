
" Calculate the area of a triangle given its base and height.
" Parameters:
" - base (int): The base length of the triangle.
" - height (int): The height of the triangle.
" Returns:
" float: The calculated area of the triangle, rounded to one decimal place.
" Example:
" >>> calculate_triangle_area(1,2)
" 1.0

function CalculateTriangleArea(base, height)
    " Calculate the area of a triangle (0.5 * base * height)
    let area = 0.5 * a:base * a:height
    
    " Round to one decimal place
    let rounded_area = str2float(substitute(string(area), '\(\.\d\)\d*$', '\1', ''))
    
    return rounded_area
endfunction

function! TestCalculateTriangleArea()
    if assert_equal(CalculateTriangleArea(1, 2), 1.0) | cq | endif
    if assert_equal(CalculateTriangleArea(3, 4), 6.0) | cq | endif
    if assert_equal(CalculateTriangleArea(5, 8), 20.0) | cq | endif
    if assert_equal(CalculateTriangleArea(7, 3), 10.5) | cq | endif
    if assert_equal(CalculateTriangleArea(10, 10), 50.0) | cq | endif
endfunction

call TestCalculateTriangleArea()
exit(0)