#include <string>
#include <algorithm>
#include <iostream>
#include <string>
#include <cassert>
/**
 * Generates a unique identifier by interweaving two words in a specified pattern.
 * @param word1 The first word to be used in the ID creation process.
 * @param word2 The second word to be used in the ID creation process.
 * @return A unique identifier formed by alternating characters from the second word,
 *         reversed, with characters from the first word.
 * Example Usage:
 * 
 * create_id("fish", "cat") returns "ftiasch"
 * create_id("icpc", "acm") returns "imccpac"
 * 
 */
std::string create_id(const std::string& word1, const std::string& word2) {
    std::string result;
    std::string reversed_word2 = word2;
    std::reverse(reversed_word2.begin(), reversed_word2.end());
    
    size_t max_length = std::max(word1.length(), reversed_word2.length());
    
    for (size_t i = 0; i < max_length; ++i) {
        if (i < word1.length()) {
            result += word1[i];
        }
        if (i < reversed_word2.length()) {
            result += reversed_word2[i];
        }
    }
    
    return result;
}
int main() {
    std::string id; // To hold the results from create_id

    id = create_id("fish", "cat");
    assert(id.compare("ftiasch") == 0);

    id = create_id("icpc", "acm");
    assert(id.compare("imccpac") == 0);

    id = create_id("oo", "w");
    assert(id.compare("owo") == 0);

    // Add more test samples
    id = create_id("hello", "world");
    assert(id.compare("hdellrloow") == 0);

    id = create_id("abc", "def");
    assert(id.compare("afbecd") == 0);

    id = create_id("buaanb", "nbbuaa");
    assert(id.compare("bauaauabnbbn") == 0);

    id = create_id("xtuisgood", "ilovextu");
    assert(id.compare("xuttuxiesvgooloid") == 0);

    // std::cout << "All tests passed!" << std::endl;

    return 0;
}