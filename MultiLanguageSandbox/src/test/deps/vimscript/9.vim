
" Count the number of odd integers in a given list of numbers.
" Parameters:
" - numbers (List): A list of integers to evaluate.
" Returns:
" int: The count of odd numbers in the input list.
" Example
" >>> let result = CountOddNumbers([1, 4, 3, 2, 5])
" >>> echo result
"     3

function CountOddNumbers(numbers)
    let count = 0
    for num in a:numbers
        if num % 2 != 0
            let count += 1
        endif
    endfor
    return count
endfunction

function! TestCountOddNumbers()
    if assert_equal(CountOddNumbers([1, 4, 3, 2, 5]), 3) | cq | endif
    if assert_equal(CountOddNumbers([2, 2, 0, 0]), 0) | cq | endif
    " Additional Test Sample
    if assert_equal(CountOddNumbers([7, 7, 8, 1, 9, 10]), 4) | cq | endif
endfunction

call TestCountOddNumbers()
exit(0)