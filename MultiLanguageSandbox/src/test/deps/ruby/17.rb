

# Calculates the minimum number of seat swaps required to group representatives of the same company together.
# Given a string representing a sequence of seats occupied by representatives of three companies, A, B, and T,
# the function returns the minimum number of swaps needed so that all representatives of each company are seated together.
# The input string contains only the characters 'A', 'B', and 'T', and each character represents a seat taken by a representative from the respective companies.
#
# Examples:
# >>> minimum_swaps_to_group_companies(\"TABTABBTTTT\")
# 3
# >>> minimum_swaps_to_group_companies(\"BBTAAT\")
# 1
def minimum_swaps_to_group_companies(s)
  companies = ['A', 'B', 'T']
  min_swaps = Float::INFINITY

  # Generate all possible permutations of the three companies
  permutations = companies.permutation.to_a

  permutations.each do |perm|
    # The target is to have all perm[0] first, then perm[1], then perm[2]
    count_a = s.count(perm[0])
    count_b = s.count(perm[1])
    count_c = s.count(perm[2])

    # The segments are:
    # 0..count_a-1: should be perm[0]
    # count_a..count_a+count_b-1: should be perm[1]
    # count_a+count_b..: should be perm[2]

    swaps = 0

    # Check the first segment (0..count_a-1) for non-perm[0] characters
    (0...count_a).each do |i|
      swaps += 1 if s[i] != perm[0]
    end

    # Check the second segment (count_a..count_a+count_b-1) for non-perm[1] characters
    (count_a...count_a + count_b).each do |i|
      swaps += 1 if s[i] != perm[1]
    end

    # The third segment is the rest, which should be perm[2], but we don't need to    # The total swaps is the sum of mismatches in the first two segments (the third segment's mismatches are the remaining characters not in the first two segments, which would be perm[2], but their count is already accounted for by the first two segments' counts)

    # However, the actual swaps needed is the sum of the mismatches in the first two segments, because:
    # - Any character in the first segment that is not perm[0] must be swapped with a character in another segment that is perm[0].
    # - Similarly for the second segment with perm[1].
    # - The third segment's mismatches are automatically fixed by these swaps.
    # So the total swaps is (mismatches in first segment) + (mismatches in second segment).

    min_swaps = [min_swaps, swaps].min
  end

  min_swaps
end

raise 'Test failed' unless minimum_swaps_to_group_companies("TABTABBTTTT") == 3
raise 'Test failed' unless minimum_swaps_to_group_companies("BBTAAT") == 1
raise 'Test failed' unless minimum_swaps_to_group_companies("AABBBTTT") == 0
raise 'Test failed' unless minimum_swaps_to_group_companies("ATBT") == 1
raise 'Test failed' unless minimum_swaps_to_group_companies("TTABAB") == 1
  



puts 'All tests passed!'