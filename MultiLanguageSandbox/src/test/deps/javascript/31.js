
/**
 * Given a string consisting of lowercase English letters, you can change one character to another
 * in each round. This function calculates the minimum number of rounds needed to make the string
 * composed of the same character.
 * @param {string} s - The input string.
 * @returns {number} The minimum number of rounds to make all characters in the string the same.
 *
 * Examples:
 * minRoundsToSameChar("aab") // returns 1
 * minRoundsToSameChar("abc") // returns 2
 * minRoundsToSameChar("aaa") // returns 0
 */

function minRoundsToSameChar(s) {
    const frequency = {};
    let maxFrequency = 0;
    
    for (const char of s) {
        if (frequency[char]) {
            frequency[char]++;
        } else {
            frequency[char] = 1;
        }
        if (frequency[char] > maxFrequency) {
            maxFrequency = frequency[char];
        }
    }
    
    return s.length - maxFrequency;
}
const testMinRoundsToSameChar = () => {
    console.assert(minRoundsToSameChar("aab") === 1, "Expected 1 round for 'aab'");
    console.assert(minRoundsToSameChar("abc") === 2, "Expected 2 rounds for 'abc'");
    console.assert(minRoundsToSameChar("aaa") === 0, "Expected 0 rounds for 'aaa'");
    console.assert(minRoundsToSameChar("abab") === 1, "Expected 1 round for 'abab'");
    console.assert(minRoundsToSameChar("zzzzz") === 0, "Expected 0 rounds for 'zzzzz'");
};

testMinRoundsToSameChar();