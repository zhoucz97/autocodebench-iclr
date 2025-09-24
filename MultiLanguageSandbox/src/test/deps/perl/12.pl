
use strict;
use warnings;

sub is_alien_sorted {
    my ($words, $order) = @_;
    
    # Create a hash to map each character to its position in the alien order
    my %order_map;
    for my $i (0..length($order)-1) {
        my $char = substr($order, $i, 1);
        $order_map{$char} = $i;
    }
    
    # Compare each word with the next one
    for my $i (0..$#$words-1) {
        my $word1 = $words->[$i];
        my $word2 = $words->[$i+1];
        
        # Compare character by character
        my $min_len = length($word1) < length($word2) ? length($word1) : length($word2);
        my $j = 0;
        while ($j < $min_len) {
            my $c1 = substr($word1, $j, 1);
            my $c2 = substr($word2, $j, 1);
            
            if ($order_map{$c1} < $order_map{$c2}) {
                last;  # words are in correct order
            } elsif ($order_map{$c1} > $order_map{$c2}) {
                return 0;  # words are out of order
            }
            $j++;
        }
        
        # If all compared characters are equal, check word lengths
        if ($j == $min_len && length($word1) > length($word2)) {
            return 0;  # first word is longer than second word
        }
    }
    
    return 1;  # all words are in correct order
}

# Test cases
print is_alien_sorted(["hello", "leetcode"], "hlabcdefgijkmnopqrstuvwxyz") ? "True\n" : "False\n";  # True
print is_alien_sorted(["word", "world", "row"], "worldabcefghijkmnpqstuvxyz") ? "True\n" : "False\n";  # False
print is_alien_sorted(["apple", "app"], "abcdefghijklmnopqrstuvwxyz") ? "True\n" : "False\n";  # False


sub check {
my $func = shift;
die "Test failed!" unless &$func(["hello", "leetcode"], "hlabcdefgijkmnopqrstuvwxyz");
die "Test failed!" if &$func(["word", "world", "row"], "worldabcefghijkmnpqstuvxyz");
die "Test failed!" if &$func(["apple", "app"], "abcdefghijklmnopqrstuvwxyz");
die "Test failed!" unless &$func(["apple"], "abcdefghijklmnopqrstuvwxyz");
print "All tests passed!\n";
}

check(\&is_alien_sorted);