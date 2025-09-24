
/**
 * Calculates the year Y_n when the n-th event occurs, considering that each
 * event has a periodicity of a_i years. The countdown for event i+1 starts
 * the year after event i occurs.
 *
 * @param n - the total number of events
 * @param signs - an array representing the periodicities of the events
 * @returns The year when the n-th event occurs
 * 
 * Example:
 * apocalypseYear(6, [3,2,4,5,9,18]) // returns 36
 */

const testApocalypseYear = (): void => {
    console.assert(apocalypseYear(6, [3, 2, 4, 5, 9, 18]) === 36);
    console.assert(apocalypseYear(5, [1, 2, 3, 4, 5]) === 5);
    console.assert(apocalypseYear(5, [1, 1, 1, 1, 1]) === 5);
    console.assert(apocalypseYear(6, [50, 30, 711, 200, 503, 1006]) === 2012);
    console.assert(apocalypseYear(2, [1, 2]) === 2);
    console.assert(apocalypseYear(3, [3, 1, 2]) === 6);
    console.assert(apocalypseYear(3, [2, 3, 4]) === 4);
    console.assert(apocalypseYear(4, [1, 2, 3, 4]) === 4);
    console.assert(apocalypseYear(4, [5, 7, 11, 13]) === 13);
    console.assert(apocalypseYear(5, [2, 2, 2, 2, 2]) === 10);
    console.assert(apocalypseYear(3, [6, 10, 15]) === 15);
    console.assert(apocalypseYear(3, [4, 6, 14]) === 14);
    console.assert(apocalypseYear(4, [50, 30, 711, 200]) === 800);
    console.assert(apocalypseYear(6, [1, 1, 1, 1, 1, 1]) === 6);
    console.assert(apocalypseYear(2, [1000000, 999999]) === 1999998);
};

testApocalypseYear();