import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.DayOfWeek;
import java.util.*;
import java.text.SimpleDateFormat;
import java.text.ParseException;

class Solution {
    /**
     * Given a date in the format "MM dd yyyy", return the weekday of the date.
     * 
     * Example:
     * >>> convertDatesToWeekdays("11 15 1997")
     * "November 15, 1997 is a Saturday"
     * >>> convertDatesToWeekdays("01 01 2000")
     * "January 1, 2000 is a Saturday"
     * >>> convertDatesToWeekdays("07 04 1998")
     * "July 4, 1998 is a Saturday"
     */
    public static String convertDatesToWeekdays(String date) 
{
        // Parse the input date string into a LocalDate object
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("MM dd yyyy");
        LocalDate localDate = LocalDate.parse(date, formatter);
        
        // Get the month name, day, and year
        String month = localDate.getMonth().toString();
        int day = localDate.getDayOfMonth();
        int year = localDate.getYear();
        
        // Get the day of the week
        DayOfWeek dayOfWeek = localDate.getDayOfWeek();
        
        // Format the output string
        return String.format("%s %d, %d is a %s", 
                            month, 
                            day, 
                            year, 
                            dayOfWeek);
    }
    public static void main(String[] args) {
        // Test the function with different test cases
        assert convertDatesToWeekdays("02 05 2024").equals("February 5, 2024 is a Monday");
        assert convertDatesToWeekdays("01 12 2023").equals("January 12, 2023 is a Thursday");
        assert convertDatesToWeekdays("09 14 2013").equals("September 14, 2013 is a Saturday");
        System.out.println("All tests passed");
    }
}