
sub sort_array_by_parity {
    my ($nums) = @_;
    my @evens = grep { $_ % 2 == 0 } @$nums;
    my @odds = grep { $_ % 2 != 0 } @$nums;
    
    my @result;
    my $even_idx = 0;
    my $odd_idx = 0;
    
    for (my $i = 0; $i < @$nums; $i++) {
        if ($i % 2 == 0) {
            $result[$i] = $evens[$even_idx++];
        } else {
            $result[$i] = $odds[$odd_idx++];
        }
    }
    
    return \@result;
}


sub check {
my $func = shift;
my @result1 = @{$func->([4, 2, 5, 7])};
die "Test failed!" unless is_valid_result(\@result1);

my @result2 = @{$func->([3, 1, 4, 2])};
die "Test failed!" unless is_valid_result(\@result2);

print "All tests passed!\n";
}

sub is_valid_result {
my ($result) = @_;
for (my $i = 0; $i < @$result; $i++) {
return 0 if $result->[$i] % 2 != $i % 2;
}
return 1;
}

check(\&sort_array_by_parity);