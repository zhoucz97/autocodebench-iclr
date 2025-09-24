
use strict;
use warnings;

sub base_addition {
    my ($base, $num1, $num2) = @_;
    
    # Convert both numbers to decimal
    my $decimal1 = 0;
    my $decimal2 = 0;
    
    # Convert num1 to decimal
    for my $i (0 .. length($num1) - 1) {
        my $char = substr($num1, $i, 1);
        my $value;
        if ($char =~ /[0-9]/) {
            $value = ord($char) - ord('0');
        } else {
            $value = uc($char) - ord('A') + 10;
        }
        $decimal1 = $decimal1 * $base + $value;
    }
    
    # Convert num2 to decimal
    for my $i (0 .. length($num2) - 1) {
        my $char = substr($num2, $i, 1);
        my $value;
        if ($char =~ /[0-9]/) {
            $value = ord($char) - ord('0');
        } else {
            $value = uc($char) - ord('A') + 10;
        }
        $decimal2 = $decimal2 * $base + $value;
    }
    
    # Sum the decimal values
    my $sum_decimal = $decimal1 + $decimal2;
    
    # Handle the case where sum is 0
    return '0' if $sum_decimal == 0;
    
    # Convert the sum back to the original base
    my @digits;
    while ($sum_decimal > 0) {
        my $remainder = $sum_decimal % $base;
        push @digits, $remainder < 10 ? $remainder : chr(ord('A') + $remainder - 10);
        $sum_decimal = int($sum_decimal / $base);
    }
    
    # The digits are in reverse order
    my $result = join('', reverse @digits);
    
    return $result;
}

# Test cases
print base_addition(2, '1010', '1101'), "\n";    # Output: 10111
print base_addition(16, '1A3', 'F2'), "\n";      # Output: 295
print base_addition(10, '123', '456'), "\n";     # Output: 579


sub test_base_addition {
die "Test failed: base 2" unless base_addition(2, '1010', '1101') eq '10111';
die "Test failed: base 16" unless base_addition(16, '1A3', 'F2') eq '295';
die "Test failed: base 10" unless base_addition(10, '123', '456') eq '579';
die "Test failed: base 36" unless base_addition(36, '1Z', '2Y') eq '4X';
die "Test failed: base 5" unless base_addition(5, '123', '432') eq '1110';
print "All tests passed!\n";
}

test_base_addition();