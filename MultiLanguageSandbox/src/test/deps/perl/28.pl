
sub remaining_volume {
    my ($w, $x, $h, $cuts) = @_;
    my $original_volume = $w * $x * $h;
    my $total_cut_volume = 0;
    
    foreach my $cut (@$cuts) {
        my ($x1, $y1, $z1, $x2, $y2, $z2) = @$cut;
        # Ensure the coordinates are in order (min to max)
        $x1 = $x1 < $x2 ? $x1 : $x2;
        $x2 = $x1 < $x2 ? $x2 : $x1;
        $y1 = $y1 < $y2 ? $y1 : $y2;
        $y2 = $y1 < $y2 ? $y2 : $y1;
        $z1 = $z1 < $z2 ? $z1 : $z2;
        $z2 = $z1 < $z2 ? $z2 : $z1;
        
        my $dx = $x2 - $x1 + 1;  # assuming inclusive coordinates
        my $dy = $y2 - $y1 + 1;
        my $dz = $z2 - $z1 + 1;
        my $cut_volume = $dx * $dy * $dz;
        $total_cut_volume += $cut_volume;
    }
    
    my $remaining = $original_volume - $total_cut_volume;
    return $remaining;
}


sub test_remaining_volume {
die "Test failed!" unless remaining_volume(5, 5, 5, [[1, 1, 1, 2, 2, 2]]) == 117;
die "Test failed!" unless remaining_volume(6, 6, 6, [[1, 1, 1, 3, 3, 3], [4, 4, 4, 5, 5, 5]]) == 181;
die "Test failed!" unless remaining_volume(3, 3, 3, [[1, 1, 1, 2, 2, 2], [2, 2, 2, 3, 3, 3]]) == 12;
die "Test failed!" unless remaining_volume(4, 4, 4, []) == 64;
print "All tests passed!\n";
}

test_remaining_volume();