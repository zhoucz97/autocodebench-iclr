
sub count_close_competitors {
    my ($students_scores) = @_;
    my $count = 0;
    my $n = scalar @$students_scores;
    
    for (my $i = 0; $i < $n; $i++) {
        for (my $j = $i + 1; $j < $n; $j++) {
            my $student1 = $students_scores->[$i];
            my $student2 = $students_scores->[$j];
            
            # Check subject differences
            my $chinese_diff = abs($student1->[0] - $student2->[0]);
            my $math_diff = abs($student1->[1] - $student2->[1]);
            my $english_diff = abs($student1->[2] - $student2->[2]);
            
            # Check total score difference
            my $total1 = $student1->[0] + $student1->[1] + $student1->[2];
            my $total2 = $student2->[0] + $student2->[1] + $student2->[2];
            my $total_diff = abs($total1 - $total2);
            
            # Check all conditions
            if ($chinese_diff <= 5 && $math_diff <= 5 && $english_diff <= 5 && $total_diff <= 10) {
                $count++;
            }
        }
    }
    
    return $count;
}


sub check {
die "Test failed!" unless count_close_competitors([[90, 85, 90], [95, 90, 95], [88, 90, 87]]) == 1;
die "Test failed!" unless count_close_competitors([[100, 100, 100], [95, 90, 85], [80, 85, 90]]) == 0;
die "Test failed!" unless count_close_competitors([[110, 105, 100], [102, 107, 103], [90, 90, 90], [85, 80, 85]]) == 0;
die "Test failed!" if count_close_competitors([[120, 125, 130], [100, 95, 90], [80, 85, 80]]) != 0;
die "Test failed!" if count_close_competitors([[100, 100, 100]]) != 0;
print "All tests passed!\n";
}

check();