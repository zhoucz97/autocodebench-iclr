{$ASSERTIONS ON}
(*
  Given n doors arranged in a circle, the player starts in front of door 1. Each turn,
  the player can choose a number i and pay a cost C_i to move i steps to the right and
  then open the door at that position. It is guaranteed that C_i >= C_{i+1} for 1 <= i < n.
  The task is to determine the minimum total cost required to open all doors.

  Example:
      >>> minTotalCost(3, (1, 1, 1))
      3
*)

function minTotalCost(n: Integer; var C: array of Integer): Int64;
var
  i, j, k, steps: Integer;
  visited: array of Boolean;
  totalCost: Int64;
begin
  SetLength(visited, n + 1); // 1-based indexing for doors
  for i := 1 to n do
    visited[i] := False;
  
  totalCost := 0;
  i := 1; // start at door 1
  
  while True do
  begin
    if visited[i] then
      Break;
    
    visited[i] := True;
    totalCost := totalCost + C[0]; // cost for step 1
    
    // Find the largest step j such that moving j steps from i doesn't land on an already visited door
    // But since C is non-increasing, we should use the largest possible step first to minimize cost
    // However, in a circle, moving j steps from i can land on any unvisited door if j is chosen appropriately
    // But the problem allows choosing any i each turn, not necessarily the same i each time
    // So the optimal is to use the largest possible step each time to cover new doors
    
    // Alternative approach: the minimal cost is sum of C_1 for each door, because moving 1 step each time is always possible
    // But given C is non-increasing, moving larger steps is cheaper per step, but may not cover all doors in one go
    // For example, with 3 doors and C = [1,1,1], moving 1 step each time costs 3 (optimal)
    // With C = [3,2,1], moving 3 steps first (cost 3) opens door 1, then 2 steps (cost 2) opens door 3, then 1 step (cost 1) opens door 2, total 6
    // But moving 1 step each time costs 3+2+1=6, same as above. So the minimal cost seems to be sum of C_i for each door opened once.
    // Wait, no: in the first example, moving 1 step each time costs 1+1+1=3, which matches the sample.
    // So the minimal cost is indeed the sum of C_i for each door opened once, because you can always choose to move 1 step each time, paying C_1 per door.
    // Since C is non-increasing, C_1 is the maximum, but moving larger steps may allow covering multiple doors with one move, but the cost per door covered would be higher (since C_j <= C_1 for j > 1).
    // For example, with C = [5,3,1], moving 3 steps first (cost 1) opens door 1, then 2 steps (cost 3) opens door 3, then 1 step (cost 5) opens door 2. Total 9.
    // But moving 1 step each time costs 5+3+1=9. Same.
    // So the minimal cost is n * C_1, because you can always choose to move 1 step each time.
    // But the sample input has C = [1,1,1], so 3*1=3, which matches.
    // So the function can simply return n * C[0].
    
    // However, the problem statement says C_i >= C_{i+1}, so C is non-increasing. The minimal cost is indeed n * C[0], because moving 1 step each time is always possible and costs C[0] per door.
    // Any other strategy would involve moving larger steps, which might cover multiple doors but at a higher cost per door (since C_j <= C_1 for j > 1), so the total cost would be the same or higher.
    
    // Therefore, the solution is to return n * C[0].
    Break;
  end;
  
  minTotalCost := n * C[0];
end;
procedure testMinTotalCost;
var
  costs1: array[1..5] of Integer = (4, 3, 3, 3, 3); // 1-indexed array
  costs2: array[1..3] of Integer = (1, 1, 1);
  costs3: array[1..4] of Integer = (5, 4, 3, 2);
  costs4: array[1..4] of Integer = (100, 99, 98, 97);
  costs5: array[1..6] of Integer = (10, 9, 8, 7, 6, 5);
  costs6: array[1..7] of Integer = (2, 2, 2, 2, 2, 2, 2);
  costs7: array[1..8] of Integer = (9, 7, 7, 7, 7, 7, 7, 7);
  costs8: array[1..9] of Integer = (3, 2, 2, 2, 2, 2, 2, 2, 2);
  costs9: array[1..10] of Integer = (6, 5, 5, 5, 5, 5, 5, 5, 5, 5);
  costs10: array[1..11] of Integer = (8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
begin
  Assert(minTotalCost(5, costs1) = 15);
  Assert(minTotalCost(3, costs2) = 3);
  Assert(minTotalCost(4, costs3) = 11);
  Assert(minTotalCost(4, costs4) = 391);
  Assert(minTotalCost(6, costs5) = 35);
  Assert(minTotalCost(7, costs6) = 14);
  Assert(minTotalCost(8, costs7) = 56);
  Assert(minTotalCost(9, costs8) = 18);
  Assert(minTotalCost(10, costs9) = 50);
  Assert(minTotalCost(11, costs10) = 11);
end;

begin
  testMinTotalCost;
end.