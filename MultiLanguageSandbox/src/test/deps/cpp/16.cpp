
#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstring>
/**
 * Count the number of valid coin toss sequences with no consecutive heads in n tosses.
 *
 * Parameters:
 * - n (int): The number of coin tosses.
 *
 * Returns:
 * - unsigned long long: The count of valid sequences.
 *
 * Examples:
 *   count_valid_coin_toss_sequences(1) returns 2
 *   count_valid_coin_toss_sequences(2) returns 3
 */
unsigned long long count_valid_coin_toss_sequences(int n) {
    if (n == 0) return 0;
    if (n == 1) return 2;
    
    unsigned long long dp0 = 1; // sequences ending with T (n=1)
    unsigned long long dp1 = 1; // sequences ending with H (n=1)
    
    for (int i = 2; i <= n; ++i) {
        unsigned long long new_dp0 = dp0 + dp1; // sequences ending with T: previous could be T or H
        unsigned long long new_dp1 = dp0;       // sequences ending with H: previous must be T
        dp0 = new_dp0;
        dp1 = new_dp1;
    }
    
    return dp0 + dp1;
}
int main() {
    assert(count_valid_coin_toss_sequences(1) == 2);
    assert(count_valid_coin_toss_sequences(2) == 3);
    assert(count_valid_coin_toss_sequences(3) == 5);
    assert(count_valid_coin_toss_sequences(4) == 8); // Additional test
    assert(count_valid_coin_toss_sequences(5) == 13); // Additional test
    // Feel free to add more tests here
    assert(count_valid_coin_toss_sequences(40) == 267914296ULL); // Additional test
    assert(count_valid_coin_toss_sequences(39) == 165580141ULL);
    assert(count_valid_coin_toss_sequences(38) == 102334155ULL);
    // printf("All tests passed!\n");
    return 0;
}