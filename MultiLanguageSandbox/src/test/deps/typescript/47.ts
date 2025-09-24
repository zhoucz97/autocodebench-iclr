
/**
 * Determine if a given string is a palindrome.
 * A palindrome is a word, phrase, number, or other sequence of characters
 * that reads the same forward and backward (ignoring spaces, punctuation,
 * and capitalization).
 * 
 * Examples:
 * isPalindrome("racecar") // returns true
 */

function isPalindrome(str: string): boolean {
    // Remove all non-alphanumeric characters and convert to lowercase
    const cleanedStr = str.replace(/[^a-zA-Z0-9]/g, '').toLowerCase();
    
    // Compare the cleaned string with its reverse
    return cleanedStr === cleanedStr.split('').reverse().join('');
}

// Test cases
 // true
 // true
 // false
 // true
const testIsPalindrome = (): void => {
  console.assert(isPalindrome("A man a plan a canal Panama") === true, "Test 1 failed");
  console.assert(isPalindrome("No lemon, no melon") === true, "Test 2 failed");
  console.assert(isPalindrome("Was it a car or a cat I saw") === true, "Test 3 failed");
  console.assert(isPalindrome("Madam, in Eden, I'm Adam") === true, "Test 4 failed");
  console.assert(isPalindrome("Never odd or even") === true, "Test 5 failed");
  console.assert(isPalindrome("Eva, can I see bees in a cave") === true, "Test 6 failed");
  console.assert(isPalindrome("hello") === false, "Test 7 failed");
  console.assert(isPalindrome("GitHub") === false, "Test 8 failed");
  console.assert(isPalindrome("programming") === false, "Test 9 failed");
};

testIsPalindrome();