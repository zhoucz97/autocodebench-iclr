

# Reverses the order of words in a given sentence and reverses each word itself.
# The function takes a single argument, 'sentence', which is a string. It returns 
# a new string with each word in the sentence reversed and the order of words also reversed.
# Punctuation (spaces, commas, periods, exclamation marks, question marks) is used to identify words 
# and is removed in the returned string.
#
# Example:
# >>> reverse_words_in_sentence("hello world!")
# "dlrow olleh"
# >>> reverse_words_in_sentence("This is, a test.")
# "tset a si sihT"

def reverse_words_in_sentence(sentence)
  # Split the sentence into words, ignoring punctuation
  words = sentence.gsub(/[^\w\s]/, '').split
  
  # Reverse each word and then reverse the order of words
  reversed_words = words.map { |word| word.reverse }.reverse
  
  # Join the words back into a sentence
  reversed_words.join(' ')
end

raise 'Test failed' unless reverse_words_in_sentence("hello world!") == "dlrow olleh"
raise 'Test failed' unless reverse_words_in_sentence("This is, a test.") == "tset a si sihT"
raise 'Test failed' unless reverse_words_in_sentence("Ruby, programming!") == "gnimmargorp ybuR"
raise 'Test failed' unless reverse_words_in_sentence("Keep calm and code on.") == "no edoc dna mlac peeK"
raise 'Test failed' unless reverse_words_in_sentence("Simple sentence.") == "ecnetnes elpmiS"



puts 'All tests passed!'