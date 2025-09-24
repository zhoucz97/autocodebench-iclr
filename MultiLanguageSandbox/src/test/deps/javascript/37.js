
/**
 * Given a sequence of student IDs entering a classroom, where the IDs range from 0 to N-1,
 * calculate the total number of handshakes that occur. Each student will shake hands with 
 * every student already in the classroom who has a smaller ID number. The sequence represents 
 * the order in which students enter the classroom.
 *
 * Examples:
 *    countHandshakes(3, [2, 1, 0])  // returns 0
 */

function countHandshakes(n, order) {
    let present = new Set();
    let totalHandshakes = 0;
    
    for (let id of order) {
        for (let existingId of present) {
            if (existingId < id) {
                totalHandshakes++;
            }
        }
        present.add(id);
    }
    
    return totalHandshakes;
}
(() => {
    console.assert(countHandshakes(4, [2, 1, 3, 0]) === 2, "Test 1 failed");
    console.assert(countHandshakes(6, [0, 1, 2, 3, 4, 5]) === 15, "Test 2 failed");
    console.assert(countHandshakes(3, [1, 2, 0]) === 1, "Test 3 failed");
    console.assert(countHandshakes(4, [3, 2, 1, 0]) === 0, "Test 4 failed");
    console.assert(countHandshakes(4, [0, 1, 2, 3]) === 6, "Test 5 failed");
    console.assert(countHandshakes(6, [5, 4, 3, 2, 1, 0]) === 0, "Test 6 failed");
    console.assert(countHandshakes(4, [0, 2, 1, 3]) === 5, "Test 7 failed");
    console.assert(countHandshakes(5, [3, 1, 4, 2, 0]) === 3, "Test 8 failed");
    console.assert(countHandshakes(4, [1, 0, 3, 2]) === 4, "Test 9 failed");
    console.assert(countHandshakes(3, [2, 0, 1]) === 1, "Test 10 failed");
    console.assert(countHandshakes(5, [1, 3, 0, 2, 4]) === 7, "Test 11 failed");
    console.assert(countHandshakes(5, [4, 3, 2, 1, 0]) === 0, "Test 12 failed");
})();