
/**
 * Shifts all characters by 5 positions in alphabetical order. Only letters
 * are replaced, and all letters are in uppercase.
 * 
 * @param text The input text to be processed.
 * @returns The transformed text with characters shifted by 5 positions.
 * 
 * Example:
 * shiftCharacters("NS BFW, JAJSYX TK NRUTWYFSHJ FWJ YMJ WJXZQY TK YWNANFQ HFZXJX")
 * returns "IN WAR, EVENTS OF IMPORTANCE ARE THE RESULT OF TRIVIAL CAUSES"
 */

function shiftCharacters(text: string): string {
    let result = '';
    for (const char of text) {
        if (char >= 'A' && char <= 'Z') {
            // Calculate the new character after shifting 5 positions
            const originalCharCode = char.charCodeAt(0);
            const shiftedCharCode = ((originalCharCode - 'A'.charCodeAt(0) - 5) % 26 + 26) % 26 + 'A'.charCodeAt(0);
            result += String.fromCharCode(shiftedCharCode);
        } else {
            result += char;
        }
    }
    return result;
}
function testShiftCharacters() {
    let test1 = "NS BFW, JAJSYX TK NRUTWYFSHJ FWJ YMJ WJXZQY TK YWNANFQ HFZXJX";
    let test2 = "N BTZQI WFYMJW GJ KNWXY NS F QNYYQJ NGJWNFS ANQQFLJ YMFS XJHTSI NS WTRJ";
    let test3 = "IFSLJW PSTBX KZQQ BJQQ YMFY HFJXFW NX RTWJ IFSLJWTZX YMFS MJ";

    console.assert(shiftCharacters(test1) === "IN WAR, EVENTS OF IMPORTANCE ARE THE RESULT OF TRIVIAL CAUSES", "Test 1 failed");
    console.assert(shiftCharacters(test2) === "I WOULD RATHER BE FIRST IN A LITTLE IBERIAN VILLAGE THAN SECOND IN ROME", "Test 2 failed");
    console.assert(shiftCharacters(test3) === "DANGER KNOWS FULL WELL THAT CAESAR IS MORE DANGEROUS THAN HE", "Test 3 failed");
}

testShiftCharacters();