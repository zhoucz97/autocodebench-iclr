
sub max_pens {
    my ($a, $b) = @_;
    
    # Convert the total money to Jiao (1 Yuan = 10 Jiao)
    my $total_jiao = $a * 10 + $b;
    
    # Price of one pen in Jiao
    my $pen_price_jiao = 19;  # 1.9 Yuan = 19 Jiao
    
    # Calculate the maximum number of pens
    my $max_pens = int($total_jiao / $pen_price_jiao);
    
    return $max_pens;
}

# Example usage:
# print max_pens(5, 0);  # Output: 2


sub check {
    my $func = shift;
    die "Test failed for input (5, 0): expected 2 got " . &$func(5, 0) unless &$func(5, 0) == 2;
    die "Test failed for input (3, 8): expected 2 got " . &$func(3, 8) unless &$func(3, 8) == 2;
    die "Test failed for input (20, 5): expected 10 got " . &$func(20, 5) unless &$func(20, 5) == 10;
    die "Test failed for input (0, 9): expected 0 got " . &$func(0, 9) unless &$func(0, 9) == 0;
}

check(\&max_pens);