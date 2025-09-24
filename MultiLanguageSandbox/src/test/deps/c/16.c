
#include <assert.h>
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
/*
Count the number of valid coin toss sequences with no consecutive heads in n tosses.
​    Parameters:
- n (int): The number of coin tosses.
​    Returns:
​    unsigned long long: The count of valid sequences.
    >>> count_valid_coin_toss_sequences(1)
    2
*/
unsigned long long count_valid_coin_toss_sequences(int n) {
    if (n == 1) {
        return 2;
    }
    if (n == 2) {
        return 3;
    }
    
    unsigned long long dp[n + 1];
    dp[1] = 2;
    dp[2] = 3;
    
    for (int i = 3; i <= n; ++i) {
        dp[i] = dp[i - 1] + dp[i - 2];
    }
    
    return dp[n];
}
int main() {
    assert(count_valid_coin_toss_sequences(1) == 2);
    assert(count_valid_coin_toss_sequences(2) == 3);
    assert(count_valid_coin_toss_sequences(3) == 5);
    assert(count_valid_coin_toss_sequences(4) == 8); // Additional test
    assert(count_valid_coin_toss_sequences(5) == 13); // Additional test
    // Feel free to add more tests here
    assert(count_valid_coin_toss_sequences(40) == 267914296); // Additional test
    assert(count_valid_coin_toss_sequences(39) == 165580141); 
    assert(count_valid_coin_toss_sequences(38) == 102334155);
    // printf("All tests passed!\n");
    return 0;
}