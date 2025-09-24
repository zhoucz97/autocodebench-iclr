#!/usr/bin/perl
use strict;
use warnings;

sub quicksort {
    my ($list) = @_;
    
    return [] if !defined $list;
    return $list if @$list <= 1;
    
    my $pivot = $list->[0];
    my @rest = @$list[1..$#$list];
    my @less = grep { $_ < $pivot } @rest;
    my @greater = grep { $_ >= $pivot } @rest;
    
    return [@{quicksort(\@less)}, $pivot, @{quicksort(\@greater)}];
}

sub arrays_equal {
    my ($a, $b) = @_;
    return 0 if @$a != @$b;
    for (my $i = 0; $i < @$a; $i++) {
        return 0 if $a->[$i] != $b->[$i];
    }
    return 1;
}

sub check_quicksort {
    die unless arrays_equal(quicksort([3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]), [1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 9]);
    die unless arrays_equal(quicksort([5, 4, 3, 2, 1]), [1, 2, 3, 4, 5]);
    die unless arrays_equal(quicksort([]), []);
    die unless arrays_equal(quicksort([1]), [1]);
}

check_quicksort();
