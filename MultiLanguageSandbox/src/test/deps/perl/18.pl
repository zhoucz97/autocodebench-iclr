
sub add_to_array_form {
    my ($num, $k) = @_;
    
    # Convert the array to a string to handle it as a number
    my $num_str = join('', @$num);
    
    # Add k to the number
    my $sum = $num_str + $k;
    
    # Convert the result back to an array of digits
    my @result = split //, $sum;
    
    return \@result;
}
```

However, this simple approach may not work for very large numbers (like the third test case with 10 nines) because Perl's native numeric operations might convert the number to floating-point, losing precision. For such cases, we need to handle the addition manually digit by digit.

Here's a more robust version that handles large numbers correctly:

```perl
sub add_to_array_form {
    my ($num, $k) = @_;
    
    # Start from the end of the array (least significant digit)
    my $i = $#{$num};
    my $carry = 0;
    
    # Process each digit from right to left
    while ($i >= 0 || $k > 0 || $carry > 0) {
        my $digit = $i >= 0 ? $num->[$i] : 0;
        my $k_digit = $k % 10;
        
        my $sum = $digit + $k_digit + $carry;
        $carry = int($sum / 10);
        $num->[$i] = $sum % 10 if $i >= 0;
        
        $k = int($k / 10);
        $i--;
    }
    
    # If there's remaining carry, prepend it
    unshift @$num, $carry while $carry > 0;
    
    return $num;
}


sub check {
my $func = shift;
die "Test failed!" unless join(',', @{$func->([1, 3, 2, 1], 9)}) eq '1,3,3,0';
die "Test failed!" unless join(',', @{$func->([2, 7, 4], 181)}) eq '4,5,5';
die "Test failed!" unless join(',', @{$func->([9, 9, 9, 9, 9, 9, 9, 9, 9, 9], 1)}) eq '1,0,0,0,0,0,0,0,0,0,0';
die "Test failed!" unless join(',', @{$func->([0], 23)}) eq '2,3';
print "All tests passed!\n";
}

check(\&add_to_array_form);