
sub max_lights_to_turn_off {
    my ($positions, $dist) = @_;
    my @sorted_positions = sort { $a <=> $b } @$positions;
    my $n = scalar @sorted_positions;
    return 0 if $n <= 2;  # Only two lights or fewer: cannot turn any off
    
    my $count = 0;
    my $i = 1;  # Start from the second light (index 1)
    
    while ($i < $n - 1) {
        my $current_pos = $sorted_positions[$i];
        my $prev_pos = $sorted_positions[$i - 1];
        my $next_pos = $sorted_positions[$i + 1];
        
        if ($current_pos - $prev_pos < $dist && $next_pos - $current_pos < $dist) {
            $count++;
            $i += 2;  # Skip the next light to avoid overlap
        } else {
            $i++;
        }
    }
    
    return $count;
}
```

Wait, but the example given is [1, 4, 5, 7, 10] with dist 3. Let's see:

Sorted positions: [1, 4, 5, 7, 10].

The first and last (1 and 10) must stay on. So we check positions 4, 5, 7.

- For position 4 (i=1):
   - prev_pos is 1, next_pos is 5.
   - 4 - 1 = 3 (not less than dist 3? No, because the condition is distance < dist. So 3 < 3 is false. So this doesn't meet the condition.
   
- For position 5 (i=2):
   - prev_pos is 4, next_pos is 7.
   - 5 - 4 = 1 < 3, and 7 - 5 = 2 < 3. So this meets the condition. We can turn off 5. Increment count to 1, and skip the next light (7), so i becomes 4. Loop ends.

So the count is 1. But the example says returns 0. Hmm, maybe the condition is that the distance between the two adjacent lights (the ones being turned off) must be less than dist, and there's a light on between them. But in the example, turning off 5 would leave 4 and 7, whose distance is 3, which is not less than dist (3). So perhaps the condition is that the distance between the two adjacent lights being turned off must be less than dist, and there's a light on between them. But in the example, turning off 4 and 5: their distance is 1 < 3, but there's no light between them (since they are adjacent). So perhaps the correct interpretation is that two adjacent lights can be turned off if the distance between them is less than dist, and there's at least one light on between them. But in the example, between 4 and 5, there's no light (they are adjacent). So no such pair exists. Hence, zero.

So the initial approach may need adjustment. Let me re-examine the problem statement.

The problem says: "Two adjacent lights can be turned off if the distance between them is less than a given threshold (dist), and there is a light on in between them."

But in the example, the positions are 1,4,5,7,10. The adjacent pairs are (1,4), (4,5), (5,7), (7,10). 

- (1,4): distance 3. Not less than dist (3). Can't turn off.
- (4,5): distance 1 < 3. But are there lights on between them? No, they are adjacent. So no light between them. So can't turn off.
- (5,7): distance 2 < 3. Lights between them? No, adjacent. So can't turn off.
- (7,10): distance 3. Not less than dist. Can't turn off.

So indeed, no lights can be turned off. Hence, the function should return 0.

So the initial approach may have misunderstood the condition. The correct condition is that two adjacent lights can be turned off if the distance between them is less than dist, and there is at least one light on between them. But adjacent lights by definition have no lights between them. So the only way to turn off lights is if there are three consecutive lights where the outer two are turned off, and the middle one stays on. But the distance between the outer two must be less than dist.

For example, if the positions were 1, 2, 3, 4, 5 with dist 2:

- The pair (1,3): distance 2. Not less than dist (2). Can't turn off.
- (2,4): distance 2. Not less than dist. Can't turn off.
- (3,5): distance 2. Not less than dist. Can't turn off.
So zero.

But if positions were 1, 2, 4, 5, dist 2:

- (1,4): distance 3. No.
- (2,4): distance 2. No.
- (4,5): distance 1 < 2. But no light between them. So can't turn off adjacent lights. But can we turn off 2 and 4? Their distance is 2, not less than dist. So no.

Hmm, maybe the correct interpretation is that you can turn off two lights that are not necessarily adjacent, but have a light on between them, and the distance between them is less than dist. For example, in 1,4,5,7,10:

- 4 and 7: distance 3. Not less than dist.
- 5 and 7: distance 2 < 3. But no light between them (they are adjacent). So can't turn off.
- 4 and 5: adjacent, distance 1 < 3, but no light between them. Can't turn off.

So zero.

Thus, the function should return 0 for the example.

So the initial approach was incorrect. The correct approach is to find all possible pairs of lights (i, j) where i < j, the distance between them is less than dist, and there is at least one light on between them (i.e., there exists a k where i < k < j and the light at k is on). Then, we can turn off i and j, provided that turning them off doesn't interfere with other pairs.

But this is more complex. Maybe a better approach is to model this as a graph where edges represent possible pairs that can be turned off, and then find a maximum matching in the graph where no two edges overlap (i.e., a maximum independent set of edges).

But for simplicity, perhaps a greedy approach where we process the lights from left to right, and whenever we find a pair (i, i+2) where the distance between i and i+2 is less than dist, we turn them off and skip the next light.

Let me try this approach:

Sort the positions. Then, iterate through the lights, and for each light at position i, check if the light at i+2 exists and the distance between i and i+2 is less than dist. If so, turn them off (increment count), and skip the next light (i+1).

This would work for cases like:

Positions: 1, 2, 3, 4, 5, dist 2.

- Check 1 and 3: distance 2. Not less than dist. Can't turn off.
- Check 2 and 4: distance 2. Not less than dist. Can't turn off.
- Check 3 and 5: distance 2. Not less than dist. Can't turn off.
So zero.

Positions: 1, 2, 4, 5, dist 2.

- Check 1 and 3 (4): distance 3. No.
- Check 2 and 4: distance 2. No.
- Check 4 and ... no.
So zero.

Positions: 1, 3, 4, 6, 7, dist 3.

- Check 1 and 3 (4): distance 3. No.
- Check 3 and 5 (6): distance 3. No.
- Check 4 and 6: distance 2 < 3. Yes. Turn off 3 and 6. Count 1. Skip 4.
Next i is 6 (but 6 is turned off, but in the array, the next is 7. But the algorithm would proceed to the next index.

Wait, perhaps the algorithm is:

Sort the positions. Initialize count to 0. i from 0 to n-3:

   if positions[i+2] - positions[i] < dist:

       count++

       i += 2  # skip the next light

   else:

       i++

Return count.

Let's test this with the example [1,4,5,7,10], dist 3.

Sorted: [1,4,5,7,10].

i starts at 0:

   positions[0] is 1, positions[2] is 5. 5 - 1 = 4 >= 3. So no. i becomes 1.

i=1:

   positions[1] is 4, positions[3] is 7. 7 - 4 = 3 >= 3. No. i becomes 2.

i=2:

   positions[2] is 5, positions[4] is 10. 10 - 5 = 5 >= 3. No. i becomes 3.

Loop ends. Count remains 0. Correct.

Another example: [1, 2, 3, 4, 5], dist 2.

i=0: 3 -1 =2 >=2. No. i=1.

i=1: 4 -2=2 >=2. No. i=2.

i=2: 5 -3=2 >=2. No. i=3.

Count 0.

[1, 2, 4, 5], dist 2.

i=0: 4 -1=3 >=2. No. i=1.

i=1: 5 -2=3 >=2. No. i=2.

Count 0.

[1, 3, 4, 6, 7], dist 3.

i=0: 4 -1=3 >=3. No. i=1.

i=1: 6 -3=3 >=3. No. i=2.

i=2: 7 -4=3 >=3. No. i=3.

Count 0.

[1, 2, 3, 5, 6], dist 3.

i=0: 3 -1=2 <3. Yes. count=1. i +=2 → i=2.

i=2: 6 -3=3 >=3. No. i=3.

Loop ends. Count 1.

So the algorithm seems to work.

Thus, the Perl function can be written as:

```perl
sub max_lights_to_turn_off {
    my ($positions, $dist) = @_;
    my @sorted_positions = sort { $a <=> $b } @$positions;
    my $n = scalar @sorted_positions;
    return 0 if $n <= 2;  # Only two lights or fewer: cannot turn any off
    
    my $count = 0;
    my $i = 0;
    
    while ($i < $n - 2) {
        if ($sorted_positions[$i + 2] - $sorted_positions[$i] < $dist) {
            $count++;
            $i += 2;  # Skip the next light to avoid overlap
        } else {
            $i++;
        }
    }
    
    return $count;
}
```

Testing the example:

max_lights_to_turn_off([1,4,5,7,10], 3):

sorted positions: [1,4,5,7,10].

i starts at 0:

   5 -1 =4 >=3. No. i=1.

i=1:

   7 -4=3 >=3. No. i=2.

i=2:

   10 -5=5 >=3. No. i=3.

Loop ends. Count 0. Correct.

Another test case: [1, 2, 3, 5, 6], dist 3.

sorted: [1,2,3,5,6].

i=0: 3 -1=2 <3. count=1. i=2.

i=2: 6 -3=3 >=3. No. i=3.

Loop ends. Count 1. Correct, because we can turn off 2 and 5 (distance 3 >=3, no), or 1 and 3 (distance 2 <3, but no light between them? Wait, the algorithm checks positions[i] and positions[i+2], so 1 and 3: distance 2 <3. So count increments. Then i becomes 2. Next check 3 and 5: distance 2 <3. But the algorithm checks i=2: positions[2+2] is 6. So 6 -3=3 >=3. No. So count remains 1. Which is correct, because turning off 1 and 3 leaves 2,5,6. The distance between 2 and 5 is 3, which is not less than dist. So only one pair can be turned off.

Thus, the function works.

So the final Perl function is:

```perl
sub max_lights_to_turn_off {
    my ($positions, $dist) = @_;
    my @sorted_positions = sort { $a <=> $b } @$positions;
    my $n = scalar @sorted_positions;
    return 0 if $n <= 2;  # Only two lights or fewer: cannot turn any off
    
    my $count = 0;
    my $i = 0;
    
    while ($i < $n - 2) {
        if ($sorted_positions[$i + 2] - $sorted_positions[$i] < $dist) {
            $count++;
            $i += 2;  # Skip the next light to avoid overlap
        } else {
            $i++;
        }
    }
    
    return $count;
}


sub test_max_lights_to_turn_off {
die "Test failed!" unless max_lights_to_turn_off([1, 4, 5, 7, 10], 3) == 1;
die "Test failed!" unless max_lights_to_turn_off([1, 2, 3, 6, 7], 2) == 1;
die "Test failed!" unless max_lights_to_turn_off([1, 3, 6, 8, 11, 14], 4) == 0;
die "Test failed!" if max_lights_to_turn_off([1, 2, 4, 7, 11], 2) != 0;
die "Test failed!" unless max_lights_to_turn_off([2, 5, 7, 10, 13], 3) == 0;
print "All tests passed!\n";
}

test_max_lights_to_turn_off();