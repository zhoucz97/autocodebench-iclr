<?php


/**
 * Determines if two months in a given year are "related".
 * Two months are considered related if the first day of each month falls on the same day of the week.
 * This condition is true if the difference in days between the first day of these months is divisible by 7.
 * Leap years are taken into account, as February has 29 days in a leap year.
 *
 * Input: 
 * - $year: An integer representing the year.
 * - $month1: An integer representing the first month.
 * - $month2: An integer representing the second month.
 *
 * Output: 
 * - Returns 'YES' if the months are related; 'NO' otherwise.
 *
 * Examples:
 * - areRelatedMonths(1994, 10, 9) -> 'NO'
 * - areRelatedMonths(1948, 1, 4) -> 'YES'
 */
function areRelatedMonths($year, $month1, $month2): string {

    // Function to calculate the number of days in a month
    function getDaysInMonth($year, $month) {
        return cal_days_in_month(CAL_GREGORIAN, $month, $year);
    }

    // Function to calculate the day of the week for the first day of a month
    function getFirstDayOfWeek($year, $month) {
        return date('N', mktime(0, 0, 0, $month, 1, $year));
    }

    // Calculate the day of the week for the first day of each month
    $dayOfWeek1 = getFirstDayOfWeek($year, $month1);
    $dayOfWeek2 = getFirstDayOfWeek($year, $month2);

    // If the days of the week are the same, the months are related
    if ($dayOfWeek1 === $dayOfWeek2) {
        return 'YES';
    } else {
        return 'NO';
    }
}

// Test cases


function main(){
    assert(areRelatedMonths(1994, 10, 9) === 'NO');
    assert(areRelatedMonths(1935, 12, 1) === 'NO');
    assert(areRelatedMonths(1957, 1, 9) === 'NO');
    assert(areRelatedMonths(1917, 9, 12) === 'YES');
    assert(areRelatedMonths(1948, 1, 4) === 'YES');
    
}

main();
?>