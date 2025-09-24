

# Calculates the difference in the number of victories between the most and least successful teams.
# This function takes a string, team_sequence, representing the sequence of winning teams, where each team
# is identified by a lowercase letter. It returns an integer representing the difference in the number
# of victories between the team that won the most and the team that won the least (excluding teams with zero victories).
#
# Example:
# >>> victory_gap_in_teams("abaabcaa")
# 4
# >>> victory_gap_in_teams("abcd")
# 0

def victory_gap_in_teams(team_sequence)
  # Count the occurrences of each team
  counts = Hash.new(0)
  team_sequence.each_char { |team| counts[team] += 1 }
  
  # Filter out teams with zero victories (though the problem says to exclude them)
  # But in the given examples, all teams have at least one victory
  # So we can proceed with the counts as they are
  
  return 0 if counts.empty?  # handle empty string case
  
  max_count = counts.values.max
  min_count = counts.values.min
  
  max_count - min_count
end

raise 'Test failed' unless victory_gap_in_teams("abaabcaa") == 4
raise 'Test failed' unless victory_gap_in_teams("abcd") == 0
raise 'Test failed' unless victory_gap_in_teams("a") == 0
raise 'Test failed' unless victory_gap_in_teams("aaabbbccc") == 0
raise 'Test failed' unless victory_gap_in_teams("abcdaaa") == 3

puts 'All tests passed!'