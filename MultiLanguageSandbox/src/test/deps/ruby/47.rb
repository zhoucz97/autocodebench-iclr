

# Counts the number of students who have enrolled in both courses A and B.
# This function takes four arguments: n and m are the number of students enrolled in courses A and B, respectively.
# a_n is an array of student IDs enrolled in course A, and b_m is an array of student IDs enrolled in course B.
# The function returns the count of students who have enrolled in both courses.
#
# Example:
# >>> count_common_students(5, 5, [1, 2, 3, 4, 5], [1, 3, 4, 5, 6])
# 4
# >>> count_common_students(3, 2, [1, 2, 3], [2, 4])
# 1
def count_common_students(n, m, a_n, b_m)
  # Convert arrays to sets for efficient intersection operation
  set_a = a_n.to_set
  set_b = b_m.to_set
  
  # Find the intersection of the two sets
  common_students = set_a & set_b
  
  # Return the count of common students
  common_students.size
end

raise 'Test failed' unless count_common_students(5, 5, [1, 2, 3, 4, 5], [1, 3, 4, 5, 6]) == 4
raise 'Test failed' unless count_common_students(3, 2, [1, 2, 3], [2, 4]) == 1
raise 'Test failed' unless count_common_students(4, 4, [7, 8, 9, 10], [10, 11, 12, 13]) == 1
raise 'Test failed' unless count_common_students(2, 3, [14, 15], [15, 16, 17]) == 1
raise 'Test failed' unless count_common_students(6, 5, [18, 19, 20, 21, 22, 23], [20, 21, 24, 25, 26]) == 2