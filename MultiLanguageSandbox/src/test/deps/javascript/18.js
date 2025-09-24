
/**
 * Creates an ID by combining two strings in a specific manner.
 * Parameters:
 * - word1 (string): The first string to be used in the ID.
 * - word2 (string): The second string to be used in the ID.
 * Returns:
 * string: A divine ID formed by reversing every other character of the second 
 *         string and combining it with the first string.
 * Example usage:
 *   createId('fish','cat') // returns 'ftiasch'
 *   createId('icpc','acm') // returns 'imccpac'
 */

function createId(word1, word2) {
    let modifiedWord2 = '';
    const oddChars = [];
    
    for (let i = 0; i < word2.length; i++) {
        if (i % 2 === 1) {
            oddChars.push(word2[i]);
        } else {
            modifiedWord2 += word2[i];
        }
    }
    
    // Reverse the collected odd characters and add them back
    for (let i = oddChars.length - 1; i >= 0; i--) {
        modifiedWord2 += oddChars[i];
    }
    
    return word1 + modifiedWord2;
}
function testCreateId() {
    // console.log(createId("fish", "cat"))
    console.assert(createId("fish", "cat") === "ftiasch", 'Test failed: createId("fish", "cat") should be "ftiasch"');
    console.assert(createId("icpc", "acm") === "imccpac", 'Test failed: createId("icpc", "acm") should be "imccpac"');
    console.assert(createId("oo", "w") === "owo", 'Test failed: createId("oo", "w") should be "owo"');
    console.assert(createId("hello", "world") === "hdellrloow", 'Test failed: createId("hello", "world") should be "hdellrloow"');
    console.assert(createId("abc", "def") === "afbecd", 'Test failed: createId("abc", "def") should be "afbecd"');
    console.assert(createId("buaanb", "nbbuaa") === "bauaauabnbbn", 'Test failed: createId("buaanb", "nbbuaa") should be "bauaauabnbbn"');
    console.assert(createId("xtuisgood", "ilovextu") === "xuttuxiesvgooloid", 'Test failed: createId("xtuisgood", "ilovextu") should be "xuttuxiesvgooloid"');
}

testCreateId();