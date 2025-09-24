#include <cstring>
#include <cassert>
#include <iostream>
/**
 * Given a sequence of N piles of stones numbered from 1 to N, each containing a positive number of stones, determine the winner
 * of a game played by Charlie and Dan. Charlie starts, taking turns with Dan moving stones from the leftmost non-empty pile
 * to the adjacent right pile. The one who can only move stones in the last pile loses. Assuming perfect play, find the winner.
 *
 * >>> gameWinner(3, {1, 2, 2})
 * "Dan"
 */
const char* gameWinner(int n, const int piles[]) {
    int sum = 0;
    for (int i = 0; i < n; ++i) {
        if ((i + 1) % 2 == 1) { // 1-based index: odd positions
            sum += piles[i];
        }
    }
    return (sum % 2 == 0) ? "Dan" : "Charlie";
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

    assert(std::string(gameWinner(3, test1)) == "Dan");
    assert(std::string(gameWinner(5, test2)) == "Charlie");
    assert(std::string(gameWinner(3, test3)) == "Charlie");
    assert(std::string(gameWinner(4, test4)) == "Charlie");
    assert(std::string(gameWinner(2, test5)) == "Charlie");
    assert(std::string(gameWinner(2, test6)) == "Charlie");
    assert(std::string(gameWinner(11, test7)) == "Dan");
    assert(std::string(gameWinner(10, test8)) == "Charlie");
    assert(std::string(gameWinner(10, test9)) == "Charlie");
    assert(std::string(gameWinner(10, test10)) == "Dan");

    return 0;
}