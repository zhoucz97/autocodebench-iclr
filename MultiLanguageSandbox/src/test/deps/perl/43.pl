
sub calculate_power_of_two {
    my ($n) = @_;
    return 2 ** $n;
}
```

### Explanation:
1. The function `calculate_power_of_two` takes a single argument `$n`.
2. It uses Perl's exponentiation operator `**` to compute `2 ** $n`, which is equivalent to 2 raised to the power of `$n`.
3. The result is returned directly.

### Example Usage:
```perl
print calculate_power_of_two(3);   # Output: 8
print calculate_power_of_two(5);   # Output: 32
print calculate_power_of_two(0);   # Output: 1
```

This implementation is simple and efficient for the given constraints (0 ≤ n < 31). The `**` operator in Perl handles integer exponents efficiently, and since the input is constrained to be less than 31, there's no risk of integer overflow (as `2^30` is 1,073,741,824, which fits within a 32-bit signed integer). 

If you wanted to avoid using the `**` operator (perhaps for educational purposes), you could implement it with a loop:

```perl
sub calculate_power_of_two {
    my ($n) = @_;
    my $result = 1;
    $result *= 2 for 1..$n;
    return $result;
}


sub check {
    my $func = shift;
    die "Test failed! Expected 8, got " . &$func(3) unless &$func(3) == 8;
    die "Test failed! Expected 32, got " . &$func(5) unless &$func(5) == 32;
    die "Test failed! Expected 1, got " . &$func(0) unless &$func(0) == 1;
    die "Test failed! Expected 1024, got " . &$func(10) unless &$func(10) == 1024;
    die "Test failed! Expected 64, got " . &$func(6) unless &$func(6) == 64;
}

check(\&calculate_power_of_two);