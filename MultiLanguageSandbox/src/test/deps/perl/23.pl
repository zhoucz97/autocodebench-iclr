
sub count_apples {
    my ($apples, $max_reach) = @_;
    my $stool_height = 30;
    my $count = 0;
    
    foreach my $height (@$apples) {
        if ($height <= $max_reach || $height <= $max_reach + $stool_height) {
            $count++;
        }
    }
    
    return $count;
}


sub check {
my $func = shift;
die "Test failed!" unless &$func([100, 200, 150, 140, 129, 134, 167, 198, 200, 111], 130) == 6;
die "Test failed!" unless &$func([100, 200, 150, 140, 129, 134, 167, 198, 200, 111], 120) == 6;
die "Test failed!" unless &$func([100, 200, 150, 140, 129, 134, 167, 198, 200, 111], 110) == 5;
}
check(\&count_apples);