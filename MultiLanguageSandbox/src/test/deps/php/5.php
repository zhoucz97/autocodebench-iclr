<?php
/**
 * Calculates the day number of the year for a given date.
 * This function takes a date in the format 'YYYY/MM/DD' and returns the day number
 * in the year for that date. For example, '2024/01/01' would return 1, as it is the first day of the year,
 * while '2024/12/31' would return 366 for a leap year or 365 otherwise.
 * Input: A string representing the date in 'YYYY/MM/DD' format.
 * Output: An integer representing the day number in the year.
 */

function getDayOfYear($date): int {

    // Parse the input date string into its components
    $dateParts = explode('/', $date);
    $year = (int)$dateParts[0];
    $month = (int)$dateParts[1];
    $day = (int)$dateParts[2];
    
    // Create a DateTime object for the given date
    $dateTime = new DateTime("$year-$month-$day");
    
    // Get the day of the year (1-366)
    return (int)$dateTime->format('z') + 1;
}
function main(){
    assert(getDayOfYear('2024/01/01') === 1);
    assert(getDayOfYear('2024/03/01') === 61); // Non-leap year
    assert(getDayOfYear('2024/12/31') === 366); // Leap year
    assert(getDayOfYear('2023/12/31') === 365); // Non-leap year
    
}

main();
?>