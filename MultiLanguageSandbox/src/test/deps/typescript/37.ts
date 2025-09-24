
/**
 * Calculates the total number of handshakes in a classroom where each student shakes
 * hands with every student already in the room with a smaller student ID. The student
 * IDs range from 0 to N-1, and the sequence of ID entries represents the order in which
 * they enter the classroom.
 * 
 * @param n - the number of students
 * @param order - the sequence of student IDs entering the classroom
 * @returns the total number of handshakes
 * 
 * Examples:
 *    countHandshakes(3, [2, 1, 0])  // returns 0
 */

function countHandshakes(n: number, order: number[]): number {
    let totalHandshakes = 0;
    const presentStudents = new Set<number>();
    
    for (const studentId of order) {
        // Count how many students in presentStudents have IDs less than studentId
        let count = 0;
        for (const id of presentStudents) {
            if (id < studentId) {
                count++;
            }
        }
        totalHandshakes += count;
        presentStudents.add(studentId);
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