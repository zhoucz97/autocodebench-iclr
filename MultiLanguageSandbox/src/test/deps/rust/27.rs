
use chrono::NaiveDate;


/*
  Calculates the day of the year for a given date string in the format YYYY/MM/DD.

  Arguments:
  - date: A string representing a date in the format YYYY/MM/DD.

  Returns:
  - Ok(usize): The day of the year as a number if the date is valid.
  - Err(String): An error message if the date is invalid.

  Example:
  - day_of_year("1985/1/20") returns Ok(20)
  - day_of_year("2006/3/12") returns Ok(71)
  - day_of_year("2024/2/30") returns Err("Invalid")
*/

fn day_of_year(date: &str) -> Result<usize, String> {
    // Parse the date string into a NaiveDate
    let parsed_date = NaiveDate::parse_from_str(date, "%Y/%m/%d");
    
    match parsed_date {
        Ok(date) => {
            // Calculate the day of the year (1-based)
            Ok(date.ordinal())
        },
        Err(_) => Err("Invalid".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_dates() {
        assert_eq!(day_of_year("1985/1/20"), Ok(20));
        assert_eq!(day_of_year("2006/3/12"), Ok(71));
        assert_eq!(day_of_year("2024/1/1"), Ok(1));
        assert_eq!(day_of_year("2024/12/31"), Ok(366)); // Leap year
    }

    #[test]
    fn test_invalid_dates() {
        assert_eq!(day_of_year("2024/2/30"), Err("Invalid".to_string()));
        assert_eq!(day_of_year("2023/13/1"), Err("Invalid".to_string()));
        assert_eq!(day_of_year("2023/0/1"), Err("Invalid".to_string()));
        assert_eq!(day_of_year("not_a_date"), Err("Invalid".to_string()));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
 
    #[test]
    fn main() {
        assert_eq!(day_of_year("1985/1/20"), Ok(20));
        assert_eq!(day_of_year("2006/3/12"), Ok(71));
        assert_eq!(day_of_year("2024/2/30"), Err("Invalid".to_string()));
        assert_eq!(day_of_year("2024/13/10"), Err("Invalid".to_string()));
        assert_eq!(day_of_year("2024/2/29"), Ok(60)); // Leap year case

    }
    

}