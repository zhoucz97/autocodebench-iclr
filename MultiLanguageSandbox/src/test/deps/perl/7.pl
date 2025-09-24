
sub rearrange_even_odd {
    my @nums = @_;
    my @evens;
    my @odds;
    
    foreach my $num (@nums) {
        if ($num % 2 == 0) {
            push @evens, $num;
        } else {
            push @odds, $num;
        }
    }
    
    return (@evens, @odds);
}


sub check {
my $func = shift;
my @result1 = @{$func->([3, 1, 2, 4])};
die "Test failed!" unless is_valid_result(\@result1, [3, 1, 2, 4]);

my @result2 = @{$func->([0, 1, 2])};
die "Test failed!" unless is_valid_result(\@result2, [0, 1, 2]);

print "All tests passed!\n";
}

sub is_valid_result {
my ($result, $original) = @_;
my $evens = 0;
my $seen_odd = 0;

foreach my $num (@$result) {
if ($num % 2 == 0) {
die "Odd number found after even numbers!" if $seen_odd;
$evens++;
} else {
$seen_odd = 1;
}
}

return $seen_odd && $evens == scalar(grep { $_ % 2 == 0 } @$original);
}

check(\&rearrange_even_odd);