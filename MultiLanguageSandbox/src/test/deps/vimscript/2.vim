
" Calculate the area of a triangle given its three sides.
" Parameters:
" - a (float): Length of side 'a'.
" - b (float): Length of side 'b'.
" - c (float): Length of side 'c'.
" Returns:
" - str: If the provided sides form a triangle, return the calculated area with 
"        2 decimal places. Otherwise, return "-1".
" Example:
" >>> CalculateTriangleArea(3,5,4)
"     6

function CalculateTriangleArea(a, b, c)
    " Check if the sides can form a triangle (triangle inequality theorem)
    if a:a + a:b <= a:c || a:a + a:c <= a:b || a:b + a:c <= a:a
        return "-1"
    endif

    " Calculate semi-perimeter
    let s = (a:a + a:b + a:c) / 2.0

    " Calculate area using Heron's formula
    let area = sqrt(s * (s - a:a) * (s - a:b) * (s - a:c))

    " Return area formatted to 2 decimal places
    return printf('%.2f', area)
endfunction

function! TestCalculateTriangleArea()
    " Triangle with sides 3, 5, 4 should return area 6
    if assert_equal(6.0, CalculateTriangleArea(3, 5, 4)) | cq | endif

    " Not a triangle with sides 1, 1, 4 should return "-1"
    " echo assert_equal(CalculateTriangleArea(1, 1, 4), -1)
    if assert_equal(-1, CalculateTriangleArea(1, 1, 4)) | cq | endif

    " Triangle with sides 7, 24, 25 should return area 84
    if assert_equal(84.0, CalculateTriangleArea(7, 24, 25)) | cq | endif

    " Triangle with sides 10.5, 6.2, 7.3 should return a correct area
    " For precision issues, we test if the calculated area is close enough to the expected value (Considering 2 decimal places)
    " Since vim does not have an explicit assert_almost_equal, we perform a manual approximate check.
    let area = CalculateTriangleArea(10.5, 6.2, 7.3)
    " echo area
    if abs(area - 22.15) >= 0.01
        cq
    endif

endfunction

call TestCalculateTriangleArea()
exit(0)