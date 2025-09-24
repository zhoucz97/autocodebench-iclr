
" This problem introduces a new modulo operation, denoted by the symbol "⊕". When
" calculating x ⊕ y, if x is not a multiple of y, the result is the remainder
" of x divided by y. Otherwise, divide x by y repeatedly until x is no longer
" a multiple of y, denoting the final value as x'. Then the result is the
" remainder of x' divided by y. For example, 4⊕5=4, 20⊕5=4, 100⊕5=4.
" Given a prime number p, there will be several queries. For each query, an
" integer n is given, and you need to compute the value of n! ⊕ p, where n! is
" the factorial of n, i.e., the product of all positive integers less than or
" equal to n.
"    >>> newModuloFactorial(3, 7)
"    6

function NewModuloFactorial(n, p)
    " Calculate n!
    let factorial = 1
    for i in range(1, a:n)
        let factorial *= i
    endfor

    " Custom modulo operation ⊕
    let x = factorial
    let y = a:p
    let count = 0

    " Check if x is a multiple of y
    while x % y == 0
        let x = x / y
        let count += 1
    endwhile

    if count > 0
        " x is no longer a multiple of y after division
        return x % y
    else
        " x was not a multiple of y initially
        return factorial % y
    endif
endfunction

function! TestNewModuloFactorial()
    if assert_equal(NewModuloFactorial(3, 7), 6) | cq | endif
    if assert_equal(NewModuloFactorial(10, 3), 1) | cq | endif
    if assert_equal(NewModuloFactorial(11, 7), 4) | cq | endif
    if assert_equal(NewModuloFactorial(45, 7), 1) | cq | endif
    if assert_equal(NewModuloFactorial(14, 7), 2) | cq | endif
    if assert_equal(NewModuloFactorial(1919, 10007), 3152) | cq | endif
    if assert_equal(NewModuloFactorial(810, 10007), 3679) | cq | endif
    if assert_equal(NewModuloFactorial(3, 7), 6) | cq | endif
    if assert_equal(NewModuloFactorial(10, 3), 1) | cq | endif
    if assert_equal(NewModuloFactorial(1, 2), 1) | cq | endif
    if assert_equal(NewModuloFactorial(5, 11), 10) | cq | endif
    if assert_equal(NewModuloFactorial(6, 13), 5) | cq | endif
    if assert_equal(NewModuloFactorial(8, 17), 13) | cq | endif
    if assert_equal(NewModuloFactorial(15, 19), 16) | cq | endif
    if assert_equal(NewModuloFactorial(21, 23), 1) | cq | endif
    if assert_equal(NewModuloFactorial(30, 29), 28) | cq | endif
    if assert_equal(NewModuloFactorial(100, 101), 100) | cq | endif
endfunction

call TestNewModuloFactorial()
exit(0)