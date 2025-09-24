
" Calculate the sum of even numbers in a given list.
" Parameters:
" - numbers (list): A list of integers.
" - size (int): The size of the list.
" Returns:
" int: The sum of even numbers in the input list.
" Example:
" >>> calculate_even_sum([1,4,3,2,5], 5)
" 6

function CalculateEvenSum(numbers, size) abort
    let sum = 0
    for i in range(a:size)
        if a:numbers[i] % 2 == 0
            let sum += a:numbers[i]
        endif
    endfor
    return sum
endfunction

function! TestCalculateEvenSum()
    let sample1 = [1, 4, 3, 2, 5]
    let sample2 = [2, 2, 0, 0]
    let sample3 = [7, 11, 19] " Additional test with no even numbers
    let sample4 = [12, 14, 16, 18, 20] " Additional test with all even numbers

    if assert_equal(CalculateEvenSum(sample1, 5), 6) | cq | endif
    if assert_equal(CalculateEvenSum(sample2, 4), 4) | cq | endif
    if assert_equal(CalculateEvenSum(sample3, 3), 0) | cq | endif
    if assert_equal(CalculateEvenSum(sample4, 5), 12 + 14 + 16 + 18 + 20) | cq | endif

endfunction

call TestCalculateEvenSum()
exit(0)