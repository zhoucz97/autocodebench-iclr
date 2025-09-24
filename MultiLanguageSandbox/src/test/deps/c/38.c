#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <stdio.h>
/*
Given n positive integers representing the count of each number from 1 to n, 
find the maximum sum of the mode (most frequent element) for all prefixes of 
a sequence constructed from these numbers. The mode is the largest number among 
the most frequent elements in a sequence.
    >>> maxModeSum(3, (int[]){1, 2, 3})
    17
    A sequence that reaches its maximum value is (3,2,3,1,2,2).
*/
long long maxModeSum(int n, const int* counts) {
    // Create an array to hold the sequence
    int* sequence = (int*)malloc((n * (n + 1) / 2) * sizeof(int));
    if (sequence == NULL) {
        return 0; // handle allocation failure
    }
    
    int pos = 0;
    // Fill the sequence with numbers from n down to 1, each repeated 'counts[i-1]' times
    for (int num = n; num >= 1; num--) {
        for (int i = 0; i < counts[num - 1]; i++) {
            sequence[pos++] = num;
        }
    }
    
    long long sum = 0;
    int* freq = (int*)calloc(n + 1, sizeof(int)); // 1-based indexing
    
    int max_freq = 0;
    int current_mode = 0;
    
    for (int i = 0; i < pos; i++) {
        int num = sequence[i];
        freq[num]++;
        
        if (freq[num] > max_freq) {
            max_freq = freq[num];
            current_mode = num;
        } else if (freq[num] == max_freq && num > current_mode) {
            current_mode = num;
        }
        
        sum += current_mode;
    }
    
    free(sequence);
    free(freq);
    
    return sum;
}
int main() {
    assert(maxModeSum(3, (int[]){1, 3, 2}) == 17);
    assert(maxModeSum(4, (int[]){4, 1, 2, 3}) == 37);
    assert(maxModeSum(2, (int[]){1, 1}) == 4);
    assert(maxModeSum(5, (int[]){1, 2, 3, 4, 5}) == 75);
    assert(maxModeSum(1, (int[]){100000}) == 100000);
    assert(maxModeSum(5, (int[]){5, 3, 2, 4, 1}) == 62);
    assert(maxModeSum(3, (int[]){100000, 100000, 100000}) == 900000);
    assert(maxModeSum(3, (int[]){2, 2, 5}) == 27);
    assert(maxModeSum(4, (int[]){4, 4, 4, 4}) == 64);
    assert(maxModeSum(6, (int[]){1, 2, 3, 4, 5, 6}) == 126);
    assert(maxModeSum(3, (int[]){3, 1, 2}) == 16);
    return 0;
}