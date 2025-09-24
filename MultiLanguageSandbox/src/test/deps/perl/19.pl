
sub find_town_judge {
    my ($n, $trust) = @_;
    
    # Initialize two arrays to keep track of trust counts
    my @trusted_by = (0) x ($n + 1);  # Number of people who trust each person
    my @trusts_someone = (0) x ($n + 1);  # Whether a person trusts someone
    
    foreach my $pair (@$trust) {
        my ($a, $b) = @$pair;
        $trusted_by[$b]++;
        $trusts_someone[$a] = 1;
    }
    
    # The judge must be trusted by n-1 people and trust no one
    for (my $i = 1; $i <= $n; $i++) {
        if ($trusted_by[$i] == $n - 1 && !$trusts_someone[$i]) {
            return $i;
        }
    }
    
    return -1;
}


sub check {
my $func = shift;
die "Test failed!" unless &$func(3, [[1, 3], [2, 3]]) == 3;
die "Test failed!" if &$func(3, [[1, 3], [2, 3], [3, 1]]) != -1;
die "Test failed!" if &$func(3, [[1, 2], [2, 3]]) != -1;
die "Test failed!" unless &$func(4, [[1, 3], [1, 4], [2, 3], [2, 4], [4, 3]]) == 3;
print "All tests passed!\n";
}

check(\&find_town_judge);