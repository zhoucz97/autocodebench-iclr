#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
/*
Creates an ID by combining two words in a specific manner.
​Parameters:
- word1 (char*): The first word to be used in the ID.
- word2 (char*): The second word to be used in the ID.
​Returns:
​char*: A divine ID formed by reversing every other character of the second 
        word and combining it with the first word.
e.g.:
    >>> create_id(fish,cat)
	ftiasch
*/
char* create_id(const char* word1, const char* word2) {
    // Step 1: Reverse every other character of word2
    int len2 = strlen(word2);
    char* modified_word2 = (char*)malloc(len2 + 1);
    strcpy(modified_word2, word2);
    
    // Reverse characters at even indices (0, 2, 4, ...)
    for (int i = 0; i < len2; i += 2) {
        if (i + 2 < len2) {
            // Swap characters at positions i and i+2
            char temp = modified_word2[i];
            modified_word2[i] = modified_word2[i+2];
            modified_word2[i+2] = temp;
        }
    }
    
    // Step 2: Interleave word1 and modified_word2
    int len1 = strlen(word1);
    int total_len = len1 + len2;
    char* id = (char*)malloc(total_len + 1);
    int id_index = 0;
    
    for (int i = 0; i < len1 || i < len2; i++) {
        if (i < len1) {
            id[id_index++] = word1[i];
        }
        if (i < len2) {
            id[id_index++] = modified_word2[i];
        }
    }
    id[total_len] = '\0';
    
    free(modified_word2);
    return id;
}

// Example usage:
int main() {
    char* id; // To hold the results from create_id

    id = create_id("fish", "cat");
    assert(strcmp(id, "ftiasch") == 0);
    free(id);

    id = create_id("icpc", "acm");
    assert(strcmp(id, "imccpac") == 0);
    free(id);

    id = create_id("oo", "w");
    assert(strcmp(id, "owo") == 0);
    free(id);

    // Add more test samples
    id = create_id("hello", "world");
    assert(strcmp(id, "hdellrloow") == 0);
    free(id);

    id = create_id("abc", "def");
    assert(strcmp(id, "afbecd") == 0);
    free(id);

    id = create_id("buaanb", "nbbuaa");
    assert(strcmp(id, "bauaauabnbbn") == 0);
    free(id);

    id = create_id("xtuisgood", "ilovextu");
    assert(strcmp(id, "xuttuxiesvgooloid") == 0);
    free(id);

    return 0;
}