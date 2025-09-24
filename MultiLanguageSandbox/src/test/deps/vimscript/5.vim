
" Calculate the distance between two points A (xa, ya) and B (xb, yb).
" Parameters:
" - xa (double): x-coordinate of point A.
" - ya (double): y-coordinate of point A.
" - xb (double): x-coordinate of point B.
" - yb (double): y-coordinate of point B.
"    
" Returns:
" double: The distance between points A and B.
" Example
" >>> calculate_distance(0,0,3,4)
"     5

function CalculateDistance(xa, ya, xb, yb)
    " Calculate the difference in x and y coordinates
    let dx = a:xb - a:xa
    let dy = a:yb - a:ya
    
    " Calculate the square of the differences
    let dx_sq = dx * dx
    let dy_sq = dy * dy
    
    " Sum the squares and take the square root
    let distance = sqrt(dx_sq + dy_sq)
    
    return distance
endfunction

function! TestCalculateDistance()
    if !(abs(CalculateDistance(0,0,3,4) - 5) < 0.00001) | cq | endif
    if !(abs(CalculateDistance(0,0,0,0) - 0) < 0.00001) | cq | endif
    if !(abs(CalculateDistance(-1,-1,2,2) - 4.242640687) < 0.00001) | cq | endif
    if !(abs(CalculateDistance(1.5,3.9,4.2,-1.1) - 5.68243) < 0.00001) | cq | endif
endfunction

call TestCalculateDistance()
exit(0)