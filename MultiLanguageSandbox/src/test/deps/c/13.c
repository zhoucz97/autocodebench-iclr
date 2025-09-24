
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
/*
Calculate the total score for a student based on the scores in different subjects.
    Parameters:
- subject_scores (list): A list containing the scores for each subject.
    Returns:
    int: The total score obtained by summing up the scores in all subjects.
    >>> calculate_total_score(100,100,100,100,100)
    500
*/
int calculate_total_score(int score1, int score2, int score3, int score4, int score5) {
    return score1 + score2 + score3 + score4 + score5;
}
int main()
{
    assert(calculate_total_score(100,100,100,100,100) == 500);
    assert(calculate_total_score(0,0,0,0,0) == 0);
    assert(calculate_total_score(20, 30, 40, 10, 50) == 150);
    assert(calculate_total_score(23, 45, 67, 89, 12) == 236);
    assert(calculate_total_score(5, 5, 5, 5, 5) == 25);
    return 0;
}