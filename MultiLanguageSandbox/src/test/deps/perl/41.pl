
sub calculate_absolute_value {
    my ($number) = @_;
    return $number >= 0 ? $number : -$number;
}
```

This function:
1. Takes one argument (the integer to process)
2. Uses a ternary operator to check if the number is non-negative
3. Returns the number itself if it's non-negative
4. Returns the negation of the number if it's negative (which makes it positive)
5. Handles zero correctly (returns 0)

You can test it with the examples you provided:
```perl
print calculate_absolute_value(-5);  # Output: 5
print calculate_absolute_value(3);   # Output: 3
print calculate_absolute_value(0);   # Output: 0
```

Alternatively, Perl also has a built-in `abs` function that does exactly this, so you could simply write:
```perl
sub calculate_absolute_value {
    my ($number) = @_;
    return abs($number);
}


sub check {
    my $func = shift;
    die "Test failed! Expected 5, got " . &$func(-5) unless &$func(-5) == 5;
    die "Test failed! Expected 3, got " . &$func(3) unless &$func(3) == 3;
    die "Test failed! Expected 0, got " . &$func(0) unless &$func(0) == 0;
    die "Test failed! Expected 10000, got " . &$func(-10000) unless &$func(-10000) == 10000;
    die "Test failed! Expected 999, got " . &$func(999) unless &$func(999) == 999;
}

check(\&calculate_absolute_value);