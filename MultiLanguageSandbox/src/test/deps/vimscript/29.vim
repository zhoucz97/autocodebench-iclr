
" Evaluate the grade for an input integer.
" Parameters:
" - score (int): The integer score to be evaluated.
" Returns:
"     char: The grade corresponding to the input score.
"           If the score is between 90 and 100 (inclusive), returns 'A'.
"           Otherwise, returns 'B'.
" Example
" >>> evaluate_integer_grade(90)
" A

function EvaluateIntegerGrade(score)
    if a:score >= 90 && a:score <= 100
        return 'A'
    else
        return 'B'
    endif
endfunction

function! TestEvaluateIntegerGrade()
    if assert_equal('A', EvaluateIntegerGrade(90)) | cq | endif
    if assert_equal('B', EvaluateIntegerGrade(89)) | cq | endif
    if assert_equal('A', EvaluateIntegerGrade(95)) | cq | endif
    if assert_equal('A', EvaluateIntegerGrade(100)) | cq | endif
    if assert_equal('B', EvaluateIntegerGrade(101)) | cq | endif
endfunction

call TestEvaluateIntegerGrade()
exit(0)