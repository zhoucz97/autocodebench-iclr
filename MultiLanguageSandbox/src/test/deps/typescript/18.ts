
/**
 * Creates an ID by combining two words in a specific manner.
 * Parameters:
 * - word1 (string): The first word to be used in the ID.
 * - word2 (string): The second word to be used in the ID.
 * Returns:
 *  string: A unique ID formed by reversing every other character of the second
 *          word and combining it with the first word.
 * e.g.:
 *      createId('fish', 'cat')  // returns 'ftiasch'
 */

function createId(word1: string, word2: string): string {
    let processedWord2 = '';
    for (let i = 0; i < word2.length; i++) {
        if (i % 2 === 0) {
            // Collect characters at even indices
            processedWord2 = word2[i] + processedWord2;
        } else {
            processedWord2 += word2[i];
        }
    }
    
    let id = '';
    const maxLength = Math.max(word1.length, processedWord2.length);
    for (let i = 0; i < maxLength; i++) {
        if (i < word1.length) {
            id += word1[i];
        }
        if (i < processedWord2.length) {
            id += processedWord2[i];
        }
    }
    
    return id;
}
const testCreateId = (): void => {
    let id: string;

    id = createId("fish", "cat");
    console.assert(id === "ftiasch", `Test failed: Expected ftiasch, got ${id}`);
  
    id = createId("icpc", "acm");
    console.assert(id === "imccpac", `Test failed: Expected imccpac, got ${id}`);
  
    id = createId("oo", "w");
    console.assert(id === "owo", `Test failed: Expected owo, got ${id}`);
  
    id = createId("hello", "world");
    console.assert(id === "hdellrloow", `Test failed: Expected hdellrloow, got ${id}`);
  
    id = createId("abc", "def");
    console.assert(id === "afbecd", `Test failed: Expected afbecd, got ${id}`);
  
    id = createId("buaanb", "nbbuaa");
    console.assert(id === "bauaauabnbbn", `Test failed: Expected bauaauabnbbn, got ${id}`);
    
    id = createId("xtuisgood", "ilovextu");
    console.assert(id === "xuttuxiesvgooloid", `Test failed: Expected xuttuxiesvgooloid, got ${id}`);
};

testCreateId();