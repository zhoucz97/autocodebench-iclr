

# Converts a numerical score into a corresponding letter grade based on predefined ranges.
# The function takes a single argument, score, which is an integer representing the score.
# It returns a string representing the letter grade, or an error message for invalid scores.
#
# Grade Conversion:
# 90-100: 'A'
# 80-89: 'B'
# 70-79: 'C'
# 60-69: 'D'
# 0-59: 'E'
# Any other score: 'Score is error!'
#
# Examples:
# >>> convert_score_to_grade(56)
# 'E'
# >>> convert_score_to_grade(100)
# 'A'
# >>> convert_score_to_grade(123)
# 'Score is error!'

def convert_score_to_grade(score)
  if score >= 90 && score <= 100
    'A'
  elsif score >= 80 && score <= 89
    'B'
  elsif score >= 70 && score <= 79
    'C'
  elsif score >= 60 && score <= 69
    'D'
  elsif score >= 0 && score <= 59
    'E'
  else
    'Score is error!'
  end
end

raise 'Test failed' unless convert_score_to_grade(56) == 'E'
raise 'Test failed' unless convert_score_to_grade(67) == 'D'
raise 'Test failed' unless convert_score_to_grade(100) == 'A'
raise 'Test failed' unless convert_score_to_grade(123) == 'Score is error!'
raise 'Test failed' unless convert_score_to_grade(-5) == 'Score is error!'
raise 'Test failed' unless convert_score_to_grade(85) == 'B'
  

puts 'All tests passed!'