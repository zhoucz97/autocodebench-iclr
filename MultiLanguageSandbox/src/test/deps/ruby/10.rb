

# Sorts a list of students by their scores in descending order. If two students have the same score,
# they are sorted alphabetically by their names. The function takes an array of arrays as an argument,
# where each sub-array contains a student's name and their score. The function returns an array of 
# student names sorted as per the mentioned criteria.
#
# Example:
# >>> sort_students_by_score([["aaa", 47], ["bbb", 90], ["ccc", 70]])
# ["bbb", "ccc", "aaa"]
# >>> sort_students_by_score([["alice", 85], ["bob", 85], ["charlie", 90]])
# ["charlie", "alice", "bob"]


def sort_students_by_score(student_scores)
  # Sort by score descending, then by name ascending
  student_scores.sort_by { |name, score| [-score, name] }.map { |name, score| name }
end

raise 'Test failed' unless sort_students_by_score([["aaa", 47], ["bbb", 90], ["ccc", 70]]) == ["bbb", "ccc", "aaa"]
raise 'Test failed' unless sort_students_by_score([["alice", 85], ["bob", 85], ["charlie", 90]]) == ["charlie", "alice", "bob"]
raise 'Test failed' unless sort_students_by_score([["zoe", 95], ["adam", 95], ["mia", 88]]) == ["adam", "zoe", "mia"]
raise 'Test failed' unless sort_students_by_score([["lara", 70], ["max", 70], ["sam", 70]]) == ["lara", "max", "sam"]
raise 'Test failed' unless sort_students_by_score([["john", 100]]) == ["john"]


puts 'All tests passed!'