{$ASSERTIONS ON}

uses
  SysUtils;
(*
  Given a sequence of N piles of stones numbered from 1 to N, each pile containing a positive number of stones,
  determine the game's winner between Charlie and Dan. Charlie starts, and players alternately move stones
  from the leftmost non-empty pile to the adjacent right pile. The player forced to move in the last pile loses.
  Assuming optimal play, decide who wins.
      >>> gameWinner([3, 1, 2, 2])
      "Dan"
*)

function gameWinner(n: Integer; const piles: array of Integer): AnsiString;
var
  i, moves: Integer;
  currentPiles: array of Integer;
begin
  // Initialize a copy of the piles array
  SetLength(currentPiles, n);
  for i := 0 to n - 1 do
    currentPiles[i] := piles[i];
  
  moves := 0;
  while True do
  begin
    // Find the leftmost non-empty pile
    i := 0;
    while (i < n) and (currentPiles[i] = 0) do
      Inc(i);
    
    // If all piles except possibly the last are empty, the current player loses
    if i = n - 1 then
    begin
      if moves mod 2 = 0 then
        Result := 'Dan'
      else
        Result := 'Charlie';
      Exit;
    end;
    
    // Move all stones from pile i to pile i+1
    currentPiles[i + 1] := currentPiles[i + 1] + currentPiles[i];
    currentPiles[i] := 0;
    Inc(moves);
  end;
end;
procedure TestGameWinner;
var
  test1: array[0..2] of Integer = (1, 2, 2);
  test2: array[0..4] of Integer = (5, 5, 5, 5, 5);
  test3: array[0..2] of Integer = (2, 1, 2);
  test4: array[0..3] of Integer = (3, 3, 3, 3);
  test5: array[0..1] of Integer = (1, 1);
  test6: array[0..1] of Integer = (2, 1);
  test7: array[0..10] of Integer = (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
  test8: array[0..9] of Integer = (2, 2, 2, 2, 2, 2, 2, 2, 2, 1);
  test9: array[0..9] of Integer = (10, 10, 10, 10, 10, 10, 10, 10, 10, 10);
  test10: array[0..9] of Integer = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
begin
  Assert(gameWinner(3, test1) = 'Dan');
  Assert(gameWinner(5, test2) = 'Charlie');
  Assert(gameWinner(3, test3) = 'Charlie');
  Assert(gameWinner(4, test4) = 'Charlie');
  Assert(gameWinner(2, test5) = 'Charlie');
  Assert(gameWinner(2, test6) = 'Charlie');
  Assert(gameWinner(11, test7) = 'Dan');
  Assert(gameWinner(10, test8) = 'Charlie');
  Assert(gameWinner(10, test9) = 'Charlie');
  Assert(gameWinner(10, test10) = 'Dan');
end;

begin
  TestGameWinner;
end.