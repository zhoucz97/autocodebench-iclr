
/**
 * Given a string consisting of lowercase English letters, you can change one character to another
 * in each round. This function calculates the minimum number of rounds needed to make the string
 * composed of the same character.
 * 
 * @param s The input string.
 * @returns The minimum number of rounds to make all characters in the string the same.
 * 
 * Examples:
 * minRoundsToSameChar("aab") // returns 1
 */

function minRoundsToSameChar(s: string): number {
    const frequency: { [key: string]: number } = {};
    
    for (const char of s) {
        if (frequency[char]) {
            frequency[char]++;
        } else {
            frequency[char] = 1;
        }
    }
    
    let maxFrequency = 0;
    for (const key in frequency) {
        if (frequency[key] > maxFrequency) {
            maxFrequency = frequency[key];
        }
    }
    
    return s.length - maxFrequency;
}
const testMinRoundsToSameChar = (): void => {
    console.assert(minRoundsToSameChar("aab") === 1, "Expected 1 round for 'aab'");
    console.assert(minRoundsToSameChar("abc") === 2, "Expected 2 rounds for 'abc'");
    console.assert(minRoundsToSameChar("aaa") === 0, "Expected 0 rounds for 'aaa'");
    console.assert(minRoundsToSameChar("abab") === 1, "Expected 1 round for 'abab'");
    console.assert(minRoundsToSameChar("zzzzz") === 0, "Expected 0 rounds for 'zzzzz'");
};

testMinRoundsToSameChar();