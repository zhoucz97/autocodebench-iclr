
/**
 * Shifts all characters by 5 positions in alphabetical order.
 * Only letters are replaced, and all letters are in uppercase.
 *
 * @param {string} text The input text to be processed.
 * @returns {string} The transformed text with characters shifted by 5 positions.
 *
 * @example
 * shiftCharacters("NS BFW, JAJSYX TK NRUTWYFSHJ FWJ YMJ WJXZQY TK YWNANFQ HFZXJX")
 * // returns "IN WAR, EVENTS OF IMPORTANCE ARE THE RESULT OF TRIVIAL CAUSES"
 */

function shiftCharacters(text) {
    let result = '';
    for (let i = 0; i < text.length; i++) {
        const char = text[i];
        if (char >= 'A' && char <= 'Z') {
            // Calculate the new character after shifting 5 positions
            const originalPos = char.charCodeAt(0) - 'A'.charCodeAt(0);
            const newPos = (originalPos - 5 + 26) % 26; // Subtract 5 and handle negative values by adding 26 before mod
            const newChar = String.fromCharCode(newPos + 'A'.charCodeAt(0));
            result += newChar;
        } else {
            result += char;
        }
    }
    return result;
}
const testShiftCharacters = () => {
    console.assert(shiftCharacters("NS BFW, JAJSYX TK NRUTWYFSHJ FWJ YMJ WJXZQY TK YWNANFQ HFZXJX") === "IN WAR, EVENTS OF IMPORTANCE ARE THE RESULT OF TRIVIAL CAUSES", 'Test 1 failed');
    console.assert(shiftCharacters("N BTZQI WFYMJW GJ KNWXY NS F QNYYQJ NGJWNFS ANQQFLJ YMFS XJHTSI NS WTRJ") === "I WOULD RATHER BE FIRST IN A LITTLE IBERIAN VILLAGE THAN SECOND IN ROME", 'Test 2 failed');
    console.assert(shiftCharacters("IFSLJW PSTBX KZQQ BJQQ YMFY HFJXFW NX RTWJ IFSLJWTZX YMFS MJ") === "DANGER KNOWS FULL WELL THAT CAESAR IS MORE DANGEROUS THAN HE", 'Test 3 failed');
};

testShiftCharacters();