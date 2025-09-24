#include <string.h>
#include <limits.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
/*
Given a string consisting of lowercase English letters, in each round you can change one of the characters to another character. The question is: what is the minimum number of rounds needed to make the string composed of the same character?
For example:
    >>> minRoundsToSameChar("aab")
    1
    >>> minRoundsToSameChar("abc")
    2
    >>> minRoundsToSameChar("aaa")
    0
*/
int minRoundsToSameChar(const char* s) {
    int freq[26] = {0}; // Initialize frequency array for each lowercase letter
    int len = strlen(s);
    
    // Count the frequency of each character in the string
    for (int i = 0; i < len; i++) {
        freq[s[i] - 'a']++;
    }
    
    // Find the maximum frequency among all characters
    int max_freq = 0;
    for (int i = 0; i < 26; i++) {
        if (freq[i] > max_freq) {
            max_freq = freq[i];
        }
    }
    
    // The answer is the total length minus the maximum frequency
    return len - max_freq;
}
int main() {
    assert(minRoundsToSameChar("aab") == 1);
    assert(minRoundsToSameChar("abc") == 2);
    assert(minRoundsToSameChar("aaa") == 0);
    assert(minRoundsToSameChar("abab") == 1);
    assert(minRoundsToSameChar("zzzzz") == 0);
    return 0;
}