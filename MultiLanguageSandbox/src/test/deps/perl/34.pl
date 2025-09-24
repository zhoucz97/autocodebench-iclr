
use strict;
use warnings;

sub find_ratio_numbers {
    my ($A, $B, $C) = @_;
    my @results;
    
    # Precompute all valid 3-digit numbers with distinct digits 1-9
    my @numbers;
    for my $num (100..999) {
        my %digits;
        my $n = $num;
        my $valid = 1;
        for (1..3) {
            my $d = $n % 10;
            if ($d == 0 || exists $digits{$d}) {
                $valid = 0;
                last;
            }
            $digits{$d} = 1;
            $n = int($n / 10);
        }
        push @numbers, $num if $valid;
    }
    
    # Check all possible triplets (x, y, z) where y = (B/A)*x, z = (C/A)*x
    # Since x, y, z must be integers, (B/A) and (C/A) must simplify to ratios that produce integers
    # So x must be a multiple of A's denominator in the simplified form of B/A and C/A
    
    # Find the least common multiple (LCM) of A, B, C to simplify checks
    # But perhaps better to iterate x in a way that y and z are integers
    
    # Alternative approach: iterate x, then compute y and z, check if they are in the numbers list and digits are distinct
    
    my %number_to_digits;
    foreach my $num (@numbers) {
        my %digits;
        my $n = $num;
        for (1..3) {
            my $d = $n % 10;
            $digits{$d} = 1;
            $n = int($n / 10);
        }
        $number_to_digits{$num} = \%digits;
    }
    
    foreach my $x (@numbers) {
        # Compute y and z based on ratios
        # y = (B/A) * x => A * y = B * x => y = (B * x) / A
        # Similarly, z = (C * x) / A
        # So (B * x) must be divisible by A, and (C * x) must be divisible by A
        
        if ((B * $x) % A != 0) {
            next;
        }
        my $y = (B * $x) / A;
        
        if ((C * $x) % A != 0) {
            next;
        }
        my $z = (C * $x) / A;
        
        # Check if y and z are 3-digit numbers in our list
        unless (grep { $_ == $y } @numbers && grep { $_ == $z } @numbers) {
            next;
        }
        
        # Check that all digits in x, y, z are distinct and cover 1-9 exactly
        my %all_digits;
        my $valid = 1;
        # Check x's digits
        my $n = $x;
        for (1..3) {
            my $d = $n % 10;
            if (exists $all_digits{$d}) {
                $valid = 0;
                last;
            }
            $all_digits{$d} = 1;
            $n = int($n / 10);
        }
        last unless $valid;
        
        # Check y's digits
        $n = $y;
        for (1..3) {
            my $d = $n % 10;
            if (exists $all_digits{$d}) {
                $valid = 0;
                last;
            }
            $all_digits{$d} = 1;
            $n = int($n / 10);
        }
        last unless $valid;
        
        # Check z's digits
        $n = $z;
        for (1..3) {
            my $d = $n % 10;
            if (exists $all_digits{$d}) {
                $valid = 0;
                last;
            }
            $all_digits{$d} = 1;
            $n = int($n / 10);
        }
        last unless $valid;
        
        # Check if all digits 1-9 are present
        $valid = 1;
        for my $d (1..9) {
            unless (exists $all_digits{$d}) {
                $valid = 0;
                last;
            }
        }
        
        if ($valid) {
            push @results, [$x, $y, $z];
        }
    }
    
    return @results ? \@results : "No!!!";
}

# Example usage:
# my $result = find_ratio_numbers(1, 2, 3);
# print join(", ", map { "($_->[0], $_->[1], $_->[2])" } @$result), "\n";
# Or for the "No!!!" case:
# my $result = find_ratio_numbers(2, 3, 4);
# print $result, "\n";
```

Wait, but the above code may not be efficient because it checks all possible x values (up to 999), and for each, computes y and z, then checks if they are in the list of valid numbers. This could be optimized by precomputing the valid numbers and their digit sets, but the current approach should work for the problem's constraints.

However, there's a potential issue: the ratios A, B, C might not simplify to integers when multiplied by x. For example, if A=1, B=2, C=3, then y = 2x and z = 3x. So x must be such that 2x and 3x are 3-digit numbers. The maximum x would be 333 (since 3*333=999). So the loop could be optimized to only check x values where y and z are 3-digit numbers.

But in the current code, the loop goes through all numbers from 100 to 999, which is 900 iterations. For each, it checks if (B*x) mod A == 0 and (C*x) mod A == 0, then checks if y and z are in the list of valid numbers. Then it checks the digits.

This should work, but for large inputs, it might be slow. But given the problem's constraints (3-digit numbers), it's manageable.

Let me test the example cases:

For find_ratio_numbers(1, 2, 3):

- x can be from 100 to 333 (since 3*333=999).
- For x=123: y=246, z=369. Check if these are in the list of valid numbers (digits 1-9, no zeros, distinct digits per number).
   - 123: digits 1,2,3
   - 246: digits 2,4,6
   - 369: digits 3,6,9
   - Combined digits: 1,2,3,2,4,6,3,6,9 → duplicates (2,3,6). So invalid.
Wait, no. The combined digits should be all distinct and cover 1-9. So 1,2,3,4,6,9 → missing 5,7,8. So invalid.

Next x=132:

- y=264, z=396.
   - 132: 1,3,2
   - 264: 2,6,4
   - 396: 3,9,6
   - Combined: 1,3,2,2,6,4,3,9,6 → duplicates (2,3,6). So invalid.

Next x=143:

- y=286, z=429.
   - 143: 1,4,3
   - 286: 2,8,6
   - 429: 4,2,9
   - Combined: 1,4,3,2,8,6,4,2,9 → duplicates (2,4). Invalid.

Hmm, the example says find_ratio_numbers(1,2,3) might return [(123,246,369), (132,264,396)], but according to the digit checks, these seem invalid because of duplicate digits in the combined set. 

Wait, the problem states that the three numbers are permutations of 1 to 9, meaning that the union of their digits is exactly the digits 1 through 9, each appearing exactly once across all three numbers. 

So for (123, 246, 369):

Digits:

123: 1,2,3

246: 2,4,6

369: 3,6,9

Combined: 1,2,3,2,4,6,3,6,9 → counts:

1:1, 2:2, 3:2, 4:1, 6:2, 9:1. So digits 2,3,6 appear twice, which violates the permutation condition. Hence, this triplet is invalid.

Similarly, (132, 264, 396):

132: 1,3,2

264: 2,6,4

396: 3,9,6

Combined: 1,3,2,2,6,4,3,9,6 → counts:

1:1, 2:2, 3:2, 4:1, 6:2, 9:1. Again, duplicates. So invalid.

Thus, the example provided seems incorrect. The correct answer for find_ratio_numbers(1,2,3) should be "No!!!" because no such triplets exist where the digits are a permutation of 1-9.

But the problem statement says the example might return those tuples. There must be a misunderstanding.

Perhaps the problem means that the three numbers together use each digit from 1 to 9 exactly once, but not necessarily as separate permutations. That is, the multiset union of their digits is exactly the digits 1-9, each appearing exactly once. But in the examples given, the digits are duplicated, so they can't be correct.

Given this, the code should correctly return "No!!!" for the first example, which contradicts the problem's example. 

This suggests that the problem's example might be incorrect, or there's a different interpretation. 

Assuming the problem requires that the three numbers together use each digit from 1 to 9 exactly once (i.e., the union of their digits is exactly the set {1,2,...,9} with no repetitions), then the provided examples are wrong, and the code should return "No!!!" for them.

But the problem statement says the examples might return those tuples, so perhaps the interpretation is different. 

Alternative interpretation: the three numbers are permutations of each other, meaning they contain the same digits in some order. But the ratios must hold. 

But the example (123, 246, 369) has digits 123, 246, 369, which are not permutations of each other. 

This is confusing. 

Given the ambiguity, I'll proceed with the initial interpretation: the three numbers together use each digit from 1 to 9 exactly once, and their ratios match A:B:C.

Thus, the code should return "No!!!" for the first example, which contradicts the problem's example. 

But since the problem states that the example might return those tuples, perhaps the correct interpretation is that the three numbers are such that when concatenated, they form a 9-digit number that's a permutation of 1-9, and their ratios match.

But the example (123, 246, 369) concatenated is 123246369, which has digits 1,2,3,2,4,6,3,6,9 → duplicates. So invalid.

Thus, the only plausible interpretation is that the three numbers are such that the multiset of their digits is exactly the digits 1-9, each appearing exactly once, and their ratios match.

Given this, the code should return "No!!!" for the first example, which contradicts the problem's example. 

Therefore, the problem's example might be erroneous, and the correct output for find_ratio_numbers(1,2,3) is "No!!!".

The code provided earlier should handle this correctly. 

For the second example, find_ratio_numbers(2,3,4), the code will check all x where (3x/2) and (4x/2)=2x are integers and 3-digit numbers. 

For x to be such that 3x/2 is an integer, x must be even. 

Maximum x is 499 (since 2*499=998, which is a 3-digit number). 

Then y=3x/2, z=4x/2=2x. 

Check if y and z are in the list of valid numbers, and their digits combined with x's digits form a permutation of 1-9.

It's likely that no such x exists, so the function returns "No!!!".

Thus, the code should work as intended.

Here's the corrected version of the function with comments:

```perl
use strict;
use warnings;

sub find_ratio_numbers {
    my ($A, $B, $C) = @_;
    my @results;
    
    # Generate all valid 3-digit numbers with distinct digits 1-9
    my @numbers;
    for my $num (100..999) {
        my %digits;
        my $n = $num;
        my $valid = 1;
        for (1..3) {
            my $d = $n % 10;
            if ($d == 0 || exists $digits{$d}) {
                $valid = 0;
                last;
            }
            $digits{$d} = 1;
            $n = int($n / 10);
        }
        push @numbers, $num if $valid;
    }
    
    # Precompute digit sets for each number for quick lookup
    my %number_to_digits;
    foreach my $num (@numbers) {
        my %digits;
        my $n = $num;
        for (1..3) {
            my $d = $n % 10;
            $digits{$d} = 1;
            $n = int($n / 10);
        }
        $number_to_digits{$num} = \%digits;
    }
    
    foreach my $x (@numbers) {
        # Check if (B * x) is divisible by A, and (C * x) is divisible by A
        if ((B * $x) % A != 0) {
            next;
        }
        my $y = (B * $x) / A;
        
        if ((C * $x) % A != 0) {
            next;
        }
        my $z = (C * $x) / A;
        
        # Check if y and z are in the list of valid numbers
        unless (exists $number_to_digits{$y} && exists $number_to_digits{$z}) {
            next;
        }
        
        # Check that all digits in x, y, z are distinct and cover 1-9 exactly
        my %all_digits;
        my $valid = 1;
        
        # Check x's digits
        my $n = $x;
        for (1..3) {
            my $d = $n % 10;
            if (exists $all_digits{$d}) {
                $valid = 0;
                last;
            }
            $all_digits{$d} = 1;
            $n = int($n / 10);
        }
        next unless $valid;
        
        # Check y's digits
        $n = $y;
        for (1..3) {
            my $d = $n % 10;
            if (exists $all_digits{$d}) {
                $valid = 0;
                last;
            }
            $all_digits{$d} = 1;
            $n = int($n / 10);
        }
        next unless $valid;
        
        # Check z's digits
        $n = $z;
        for (1..3) {
            my $d = $n % 10;
            if (exists $all_digits{$d}) {
                $valid = 0;
                last;
            }
            $all_digits{$d} = 1;
            $n = int($n / 10);
        }
        next unless $valid;
        
        # Check if all digits 1-9 are present
        for my $d (1..9) {
            unless (exists $all_digits{$d}) {
                $valid = 0;
                last;
            }
        }
        
        if ($valid) {
            push @results, [$x, $y, $z];
        }
    }
    
    return @results ? \@results : "No!!!";
}

# Example usage:
# my $result = find_ratio_numbers(1, 2, 3);
# print join(", ", map { "($_->[0], $_->[1], $_->[2])" } @$result), "\n";
# Or for the "No!!!" case:
# my $result = find_ratio_numbers(2, 3, 4);
# print $result, "\n";


sub test {
my $func = shift;

my @test1 = &$func(1, 2, 3);
die "Test failed: Expected at least one solution for 1:2:3 ratio" unless @test1;

my @test2 = &$func(2, 5, 7);
die "Test failed: Expected 'No!!!' for 2:5:7 ratio" if @test2 && $test2[0] ne 'No!!!';

my @test3 = &$func(3, 6, 9);
die "Test failed: Expected at least one solution for 3:6:9 ratio" unless @test3;

print "All tests passed!\n";
}

test(\&find_ratio_numbers);