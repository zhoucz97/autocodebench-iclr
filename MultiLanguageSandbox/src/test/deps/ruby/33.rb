

# Generates all unique four-digit permutations using the given four digits.
# Each digit is used exactly once in each permutation. The function accepts
# four integers as arguments and returns an array of strings, each representing
# a unique permutation. The permutations are sorted in ascending order.
#
# Example:
# >>> generate_four_digit_permutations(1, 2, 3, 4)
# ["1234", "1243", "1324", "1342", "1423", "1432", ...]
# >>> generate_four_digit_permutations(1, 2, 3, 5)
# ["1235", "1253", "1325", "1352", "1523", "1532", ...]


def generate_four_digit_permutations(*digits)
  # Ensure we have exactly four digits, "Exactly four digits must be provided" unless digits.size == 4

  # Generate all permutations of the four digits
  permutations = digits.permutation.to_a

  # Convert each permutation array to a string
  result = permutations.map { |perm| perm.join }

  # Sort the result in ascending order
  result.sort
end

raise 'Test failed' unless generate_four_digit_permutations(1, 2, 3, 4) == ["1234", "1243", "1324", "1342", "1423", "1432", "2134", "2143", "2314", "2341", "2413", "2431", "3124", "3142", "3214", "3241", "3412", "3421", "4123", "4132", "4213", "4231", "4312", "4321"]

raise 'Test failed' unless generate_four_digit_permutations(1, 2, 3, 5) == ["1235", "1253", "1325", "1352", "1523", "1532", "2135", "2153", "2315", "2351", "2513", "2531", "3125", "3152", "3215", "3251", "3512", "3521", "5123", "5132", "5213", "5231", "5312", "5321"]
   


puts 'All tests passed!'