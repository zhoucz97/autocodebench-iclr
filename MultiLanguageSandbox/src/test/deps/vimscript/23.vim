
" Count the number of acute-angled triangles that can be
" formed by selecting any 3 edges out of a given set of edges.
"
" Args:
" - edges: A list of integers representing the lengths of the edges.
"
" Returns:
" int: The count of distinct acute-angled triangles that can be formed.
"
" Examples:
" >>> CountAcuteTriangles([1, 1, 1, 1])
" 4

function CountAcuteTriangles(edges)
    let count = 0
    let n = len(a:edges)
    
    " Generate all combinations of 3 distinct edges
    for i in range(n)
        for j in range(i + 1, n - 1)
            for k in range(j + 1, n - 1)
                let a = a:edges[i]
                let b = a:edges[j]
                let c = a:edges[k]
                
                " Sort the sides to easily identify the largest side
                if a > b
                    let temp = a
                    let a = b
                    let b = temp
                endif
                if a > c
                    let temp = a
                    let a = c
                    let c = temp
                endif
                if b > c
                    let temp = b
                    let b = c
                    let c = temp
                endif
                
                " Check triangle inequality: a + b > c
                if a + b > c
                    " Check for acute triangle: a^2 + b^2 > c^2
                    if a * a + b * b > c * c
                        let count += 1
                    endif
                endif
            endfor
        endfor
    endfor
    
    return count
endfunction

function! TestCountAcuteTriangles()
    if assert_equal(CountAcuteTriangles([1, 1, 1, 1]), 4) | cq | endif
    if assert_equal(CountAcuteTriangles([1, 2, 3]), 0) | cq | endif
    " Additional tests to ensure correctness
    if assert_equal(CountAcuteTriangles([3, 4, 7, 5, 10]), 0) | cq | endif
    if assert_equal(CountAcuteTriangles([6, 8, 10, 5, 5, 5]), 4) | cq | endif
endfunction

call TestCountAcuteTriangles()
exit(0)