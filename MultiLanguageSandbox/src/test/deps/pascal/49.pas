{$ASSERTIONS ON}
(* 
  Nim Game:
  You are playing a game called Nim. In this game, you start with a pile of n stones,
  and you and your opponent take turns to remove 1 to 3 stones from the pile.
  The one who removes the last stone wins the game.
  Given the number of stones n, determine if you can win the game if both you and your
  opponent play optimally. You always take the first turn.
  
  Examples:
    >>> canWinNim(1)  returns true
*)

function canWinNim(n: integer): boolean;
begin
  canWinNim := (n mod 4) <> 0;
end;
procedure testCanWinNim;
begin
    Assert(canWinNim(1) = true);
    Assert(canWinNim(2) = true);
    Assert(canWinNim(3) = true);
    Assert(canWinNim(4) = false);
    Assert(canWinNim(5) = true);
    Assert(canWinNim(6) = true);
    Assert(canWinNim(7) = true);
    Assert(canWinNim(8) = false);
    Assert(canWinNim(9) = true);
    Assert(canWinNim(10) = true);
    Assert(canWinNim(11) = true);
    Assert(canWinNim(12) = false);
    Assert(canWinNim(13) = true);
    Assert(canWinNim(14) = true);
    Assert(canWinNim(15) = true);
    Assert(canWinNim(16) = false);
    Assert(canWinNim(17) = true);
    Assert(canWinNim(18) = true);
    Assert(canWinNim(19) = true);
    Assert(canWinNim(20) = false);
end;

begin
    testCanWinNim;
end.