
sub find_substring_index {
    my ($haystack, $needle) = @_;
    
    # Check if needle is empty (edge case)
    return 0 if $needle eq '';
    
    # Use index function which returns the position of the first occurrence
    my $index = index($haystack, $needle);
    
    # index returns -1 if not found
    return $index;
}


sub check {
my $func = shift;
die "Test 1 failed!" unless &$func("hellothere", "there") == 5;
die "Test 2 failed!" unless &$func("justatest", "test") == 5;
die "Test 3 failed!" unless &$func("nonesuch", "none") == 0;
die "Test 4 failed!" unless &$func("nonesuch", "such") == 4;
die "Test 5 failed!" unless &$func("nonesuch", "nope") == -1;
print "All tests passed!";
}

check(\&find_substring_index);