
use strict;
use warnings;

sub is_prime {
    my $num = shift;
    return 0 if $num < 2;
    for (my $i = 2; $i * $i <= $num; $i++) {
        return 0 if $num % $i == 0;
    }
    return 1;
}

sub find_larger_prime_factor {
    my $n = shift;
    
    # Find the smaller prime factor first
    for (my $i = 2; $i * $i <= $n; $i++) {
        if ($n % $i == 0 && is_prime($i)) {
            # The corresponding factor is $n / $i
            my $other_factor = $n / $i;
            return $other_factor if is_prime($other_factor);
        }
    }
    
    # If no factors found (shouldn't happen per problem statement)
    return -1;
}

# Test cases
print find_larger_prime_factor(15), "\n";  # Output: 5
print find_larger_prime_factor(33), "\n";  # Output: 11


sub check {
my $func = shift;
die "Test failed!" unless &$func(35) == 7;
die "Test failed!" unless &$func(77) == 11;
die "Test failed!" unless &$func(91) == 13;
die "Test failed!" unless &$func(143) == 13;
die "Test failed!" unless &$func(221) == 17;
die "Test failed!" unless &$func(323) == 19;
print "All tests passed!";
}

check(\&find_larger_prime_factor);