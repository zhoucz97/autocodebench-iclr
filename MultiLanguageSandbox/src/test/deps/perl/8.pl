
use List::Util qw(gcd);

sub can_divide_into_groups {
    my ($deck) = @_;
    my %count;
    
    # Count the frequency of each card
    foreach my $card (@$deck) {
        $count{$card}++;
    }
    
    # Extract the counts into an array
    my @counts = values %count;
    
    # Compute the GCD of all counts
    my $current_gcd = $counts[0];
    foreach my $cnt (@counts[1..$#counts]) {
        $current_gcd = gcd($current_gcd, $cnt);
        last if $current_gcd == 1; # Early exit if GCD becomes 1
    }
    
    return $current_gcd >= 2;
}

# Test cases
print can_divide_into_groups([1,2,3,4,4,3,2,1]) ? "True\n" : "False\n"; # True
print can_divide_into_groups([1,1,1,2,2,2,3,3]) ? "True\n" : "False\n"; # False


sub check {
my $func = shift;
die "Test failed!" unless &$func([1,2,3,4,4,3,2,1]);
die "Test failed!" if &$func([1,1,1,2,2,2,3,3]);
die "Test failed!" unless &$func([1,1,2,2,2,2]);
die "Test failed!" if &$func([1]);
print "All tests passed!\n";
}

check(\&can_divide_into_groups);