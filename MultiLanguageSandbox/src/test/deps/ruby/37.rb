

# Sorts a list of dates in the format 'MM/DD/YYYY'.
# The function takes an array of string dates and returns an array of dates sorted in ascending order.
# The sorting is done based on the chronological order of the dates.
#
# Examples:
# >>> sort_dates(["15/12/1999", "10/21/2003", "02/12/2004"])
# ["15/12/1999", "10/21/2003", "02/12/2004"]
# >>> sort_dates(["12/31/2005", "11/30/2005", "10/22/2003"])
# ["10/22/2003", "11/30/2005", "12/31/2005"]

def sort_dates(dates)
  # Convert each date string to a Date object for proper comparison
  dates.map { |date| Date.strptime(date, '%m/%d/%Y') }
       .sort
       .map { |date| date.strftime('%m/%d/%Y') }
end

raise 'Test failed' unless sort_dates(["10/21/2003", "15/12/1999",  "02/12/2004"]) == ["15/12/1999", "10/21/2003", "02/12/2004"]
raise 'Test failed' unless sort_dates(["12/31/2005", "11/30/2005", "10/22/2003"]) == ["10/22/2003", "11/30/2005", "12/31/2005"]
raise 'Test failed' unless sort_dates(["01/01/2000", "12/31/1999"]) == ["12/31/1999", "01/01/2000"]
  

puts 'All tests passed!'