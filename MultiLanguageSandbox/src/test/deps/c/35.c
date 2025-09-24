#include <stdio.h>
#include <assert.h>
#include <stdio.h>
/*
Given a sequence of N piles of stones numbered from 1 to N, with each pile containing a positive number of stones, determine the winner of a game played by Charlie and Dan. Charlie goes first, and they take turns moving a positive number of stones from the leftmost non-empty pile to the adjacent right pile. The player who can only move stones in the last pile loses. Assuming both players use the best possible strategy, find out who will win the game.
    >>> gameWinner(3, 1, 2, 2)
    "Dan"
*/
const char* gameWinner(int n, int piles[]) {
    int xor_sum = 0;
    for (int i = 0; i < n; i++) {
        xor_sum ^= piles[i];
    }
    return (xor_sum != 0) ? "Charlie" : "Dan";
}
int main() {
    int test1[] = {1, 2, 2};
    int test2[] = {5, 5, 5, 5, 5};
    int test3[] = {2, 1, 2};
    int test4[] = {3, 3, 3, 3};
    int test5[] = {1, 1};
    int test6[] = {2, 1};
    int test7[] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1};
    int test8[] = {2, 2, 2, 2, 2, 2, 2, 2, 2, 1};
    int test9[] = {10, 10, 10, 10, 10, 10, 10, 10, 10, 10};
    int test10[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

    assert(gameWinner(3, test1) == "Dan");
    assert(gameWinner(5, test2) == "Charlie");
    assert(gameWinner(3, test3) == "Charlie");
    assert(gameWinner(4, test4) == "Charlie");
    assert(gameWinner(2, test5) == "Charlie");
    assert(gameWinner(2, test6) == "Charlie");
    assert(gameWinner(11, test7) == "Dan");
    assert(gameWinner(10, test8) == "Charlie");
    assert(gameWinner(10, test9) == "Charlie");
    assert(gameWinner(10, test10) == "Dan");

    return 0;
}