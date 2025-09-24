
sub find_words_with_prefix {
    my ($words_ref, $prefix) = @_;
    my @words = @$words_ref;
    
    # Filter words that start with the given prefix
    my @matching_words = grep { index($_, $prefix) == 0 } @words;
    
    # Sort the matching words in dictionary order
    @matching_words = sort @matching_words;
    
    return @matching_words;
}


sub test_find_words_with_prefix {
use Test::More tests => 4;

is_deeply([find_words_with_prefix(['apple', 'art', 'banana', 'artist', 'arrow'], 'ar')],
['arrow', 'art', 'artist'],
'Test with prefix "ar"');

is_deeply([find_words_with_prefix(['dog', 'cat', 'cow', 'camel', 'donkey'], 'ca')],
['camel', 'cat'],
'Test with prefix "ca"');

is_deeply([find_words_with_prefix(['hello', 'high', 'hedge', 'host', 'hill'], 'ho')],
['host'],
'Test with prefix "ho"');

is_deeply([find_words_with_prefix(['blue', 'black', 'brown', 'beige'], 'br')],
['brown'],
'Test with prefix "br"');

done_testing();
}

test_find_words_with_prefix();