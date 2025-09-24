
use strict;
use warnings;

sub find_candy_exchange {
    my ($aliceSizes, $bobSizes) = @_;
    
    # Calculate total candies for Alice and Bob
    my $alice_total = 0;
    $alice_total += $_ for @$aliceSizes;
    
    my $bob_total = 0;
    $bob_total += $_ for @$bobSizes;
    
    # The difference needed after exchange is (alice_total - bob_total) / 2
    my $diff = ($alice_total - $bob_total) / 2;
    
    # Create a hash for quick lookup of Bob's candy sizes
    my %bob_sizes = map { $_ => 1 } @$bobSizes;
    
    # Iterate through Alice's candy sizes to find a matching pair
    foreach my $alice_candy (@$aliceSizes) {
        my $bob_candy = $alice_candy - $diff;
        if (exists $bob_sizes{$bob_candy}) {
            return [$alice_candy, $bob_candy];
        }
    }
    
    # If no solution found (though problem states there is one)
    return [];
}

# Test cases
my @test1 = find_candy_exchange([1, 1], [2, 2]);
print "Test 1: [", join(", ", @test1), "]\n";  # Expected: [1, 2]

my @test2 = find_candy_exchange([1, 2, 5], [2, 4]);
print "Test 2: [", join(", ", @test2), "]\n";  # Expected: [5, 4]


sub check {
my $func = shift;
use Test::More tests => 3;
is_deeply &$func([1,1], [2,2]), [1, 2], "Test 1 passed!";
is_deeply &$func([1,2,5], [2,4]), [5, 4], "Test 2 passed!";
is_deeply &$func([2], [1,3]), [2, 3], "Test 3 passed!";
}

check(\&find_candy_exchange);