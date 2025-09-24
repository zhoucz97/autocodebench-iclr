
sub reverse_output {
    my @strings = @_;
    for (my $i = $#strings; $i >= 0; $i--) {
        print "$strings[$i]\n";
    }
}
```

You can use it like this:
```perl
reverse_output("hello", "world", "Perl");
```

This will produce the output:
```
Perl
world
hello
```

The function works by:
1. Taking the input list as an array `@strings`
2. Using a loop that starts from the last index (`$#strings`) and goes down to 0
3. Printing each string followed by a newline character

Alternatively, you could also write it more concisely using Perl's built-in `reverse` function:

```perl
sub reverse_output {
    print join("\n", reverse @_), "\n";
}


sub check {
my $func = shift;
my @test1 = $func->(["hello", "world", "Perl"]);
die "Test failed!" unless join(',', @test1) eq 'Perl,world,hello';

my @test2 = $func->(["line1", "line2", "line3", "line4"]);
die "Test failed!" unless join(',', @test2) eq 'line4,line3,line2,line1';

my @test3 = $func->(["one string"]);
die "Test failed!" unless join(',', @test3) eq 'one string';

print "All tests passed!\n";
}

check(\&reverse_output);