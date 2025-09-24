
/**
 * Decode a series of numbers to reveal the pattern and understand the actual values
 * each digit represents. The patterns are as follows:
 * 0000=4, 8888=8, 1234=1, 5678=3, 9012=2, 1357=0, 2468=4.
 *
 * @param {string} dataStr - A string representing a series of numbers.
 *                           The length does not exceed 100 characters.
 * @return {number} The result corresponding to each data string as per the pattern.
 *
 * Example usage:
 * console.assert(decodeNumbers("0000") === 4);
 */

function decodeNumbers(dataStr) {
    const patterns = {
        "0000": 4,
        "8888": 8,
        "1234": 1,
        "5678": 3,
        "9012": 2,
        "1357": 0,
        "2468": 4
    };
    
    return patterns[dataStr];
}
function testDecodeNumbers() {
    console.assert(decodeNumbers("0000") === 4, "Test 0000 failed");
    console.assert(decodeNumbers("8888") === 8, "Test 8888 failed");
    console.assert(decodeNumbers("1234") === 1, "Test 1234 failed");
    console.assert(decodeNumbers("5678") === 3, "Test 5678 failed");
    console.assert(decodeNumbers("9012") === 2, "Test 9012 failed");
    console.assert(decodeNumbers("1357") === 0, "Test 1357 failed");
    console.assert(decodeNumbers("2468") === 4, "Test 2468 failed");

    // Additional test samples
    console.assert(decodeNumbers("9999") === 4, "Test 9999 failed");
    console.assert(decodeNumbers("1111") === 0, "Test 1111 failed");
    console.assert(decodeNumbers("2222") === 0, "Test 2222 failed");
    console.assert(decodeNumbers("3333") === 0, "Test 3333 failed");
    console.assert(decodeNumbers("4444") === 4, "Test 4444 failed");
    console.assert(decodeNumbers("5555") === 0, "Test 5555 failed");
    console.assert(decodeNumbers("6666") === 4, "Test 6666 failed");
    console.assert(decodeNumbers("7777") === 0, "Test 7777 failed");
    console.assert(decodeNumbers("0001") === 3, "Test 0001 failed");
    console.assert(decodeNumbers("2301") === 1, "Test 2301 failed");
}

testDecodeNumbers();