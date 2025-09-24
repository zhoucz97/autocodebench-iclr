

# Finds the longest word in a given sentence.
# The function takes a single argument, sentence, which is a string containing multiple words separated by spaces.
# Each word is composed only of lowercase letters. The function returns the longest word found in the sentence.
# If there are multiple words of the same longest length, it returns the first one encountered.
#
# Example:
# >>> find_longest_word("I am a student")
# "student"
# >>> find_longest_word("The quick brown fox")
# "quick"

def find_longest_word(sentence)
  words = sentence.split
  longest_word = words.max_by { |word| word.length }
  longest_word
end

raise 'Test failed' unless find_longest_word("I am a student") == "student"
raise 'Test failed' unless find_longest_word("The quick brown fox jumps over the lazy dog") == "quick"
raise 'Test failed' unless find_longest_word("Hello world") == "Hello"
raise 'Test failed' unless find_longest_word("A B C D EFG HI") == "EFG"
raise 'Test failed' unless find_longest_word("Ruby Python Java") == "Python"