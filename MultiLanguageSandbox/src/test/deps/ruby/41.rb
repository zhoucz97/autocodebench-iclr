

# Counts the number of pairs of students who are considered "equally matched opponents".
# Two students are "equally matched" if the difference in scores for each subject (Chinese, Mathematics, English) 
# is no more than 5 points, and the total score difference is no more than 10 points.
# The function takes an array of arrays, where each sub-array represents a student's scores in the three subjects,
# and returns the total count of "equally matched" student pairs.
#
# Example:
# >>> count_matching_pairs([[90, 90, 90], [85, 95, 90], [80, 100, 91]])
# 2


def count_matching_pairs(students_scores)
  count = 0
  n = students_scores.size

  # Iterate through all possible pairs of students
  (0...n).each do |i|
    (i+1...n).each do |j|
      student1 = students_scores[i]
      student2 = students_scores[j]

      # Check each subject's score difference
      chinese_diff = (student1[0] - student2[0]).abs
      math_diff = (student1[1] - student2[1]).abs
      english_diff = (student1[2] - student2[2]).abs

      # Check total score difference
      total_diff = (student1.sum - student2.sum).abs

      # If all conditions are met, increment the count
      if chinese_diff <= 5 && math_diff <= 5 && english_diff <= 5 && total_diff <= 10
        count += 1
      end
    end
  end

  count
end

raise 'Test failed' unless count_matching_pairs([[90, 90, 90], [85, 95, 90], [80, 100, 91]]) == 2
raise 'Test failed' unless count_matching_pairs([[100, 100, 100], [100, 100, 105], [90, 90, 90]]) == 1
raise 'Test failed' unless count_matching_pairs([[90, 90, 90], [85, 80, 85], [80, 85, 80], [76, 81, 85]]) == 2
raise 'Test failed' unless count_matching_pairs([[100, 100, 100]]) == 0
raise 'Test failed' unless count_matching_pairs([]) == 0