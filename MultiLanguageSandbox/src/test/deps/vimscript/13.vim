
" Calculate the total score for a student based on the scores in different subjects.
" Parameters:
" - subject_scores (list): A list containing the scores for each subject.
" Returns:
"     int: The total score obtained by summing up the scores in all subjects.
" Example:
" >>> CalculateTotalScore(100,100,100,100,100)
"     500

function CalculateTotalScore(score1, score2, score3, score4, score5)
    return a:score1 + a:score2 + a:score3 + a:score4 + a:score5
endfunction

function! TestCalculateTotalScore()
    if assert_equal(CalculateTotalScore(100, 100, 100, 100, 100), 500) | cq | endif
    if assert_equal(CalculateTotalScore(0, 0, 0, 0, 0), 0) | cq | endif
    if assert_equal(CalculateTotalScore(20, 30, 40, 10, 50), 150) | cq | endif
    if assert_equal(CalculateTotalScore(23, 45, 67, 89, 12), 236) | cq | endif
    if assert_equal(CalculateTotalScore(5, 5, 5, 5, 5), 25) | cq | endif
endfunction

call TestCalculateTotalScore()
exit(0)