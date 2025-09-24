

# Counts the number of characters in a given title, excluding spaces and newline characters.
# The function takes a single argument, title, which is a string representing the title of an essay.
# It returns the count of characters in the title, excluding any spaces and newline characters.
#
# Examples:
# >>> count_chars_in_title("234")
# 3
# >>> count_chars_in_title("Ca 45")
# 4
# >>> count_chars_in_title("Hello World\n")
# 10

def count_chars_in_title(title)
  # Remove all spaces and newline characters from the title
  cleaned_title = title.gsub(/[ \n]/, '')
  # Return the length of the cleaned string
  cleaned_title.length
end

raise 'Test failed' unless count_chars_in_title("234") == 3
raise 'Test failed' unless count_chars_in_title("Ca 45") == 4
raise 'Test failed' unless count_chars_in_title("Hello World\n") == 10
raise 'Test failed' unless count_chars_in_title("Ruby Programming 101 ") == 18
raise 'Test failed' unless count_chars_in_title("\nNew Line\n") == 7