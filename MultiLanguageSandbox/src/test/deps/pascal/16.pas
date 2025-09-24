{$ASSERTIONS ON}
(*
Function that counts the number of valid coin toss sequences
without consecutive heads. It calculates possible combinations
for a given number of tosses.

Parameters:
- n: an integer representing the number of coin tosses.

Returns:
The count of valid sequences as an unsigned 64-bit integer.

Examples:
>>> count_valid_coin_toss_sequences(1)
2
*)

function count_valid_coin_toss_sequences(n: integer): int64;
var
  i: integer;
  dp: array of int64;
begin
  if n = 0 then
    Exit(1);
  if n = 1 then
    Exit(2);
  
  SetLength(dp, n + 1);
  dp[0] := 1;
  dp[1] := 2;
  
  for i := 2 to n do
  begin
    dp[i] := dp[i-1] + dp[i-2];
  end;
  
  count_valid_coin_toss_sequences := dp[n];
end;
procedure test_count_valid_coin_toss_sequences;
begin
  Assert(count_valid_coin_toss_sequences(1) = 2);
  Assert(count_valid_coin_toss_sequences(2) = 3);
  Assert(count_valid_coin_toss_sequences(3) = 5);
  Assert(count_valid_coin_toss_sequences(4) = 8);
  Assert(count_valid_coin_toss_sequences(5) = 13);
  Assert(count_valid_coin_toss_sequences(40) = 267914296);
  Assert(count_valid_coin_toss_sequences(39) = 165580141);
  Assert(count_valid_coin_toss_sequences(38) = 102334155);

  (* Uncomment the line below to add output for successful tests *)
  // WriteLn('All tests passed!');
end;

begin
  test_count_valid_coin_toss_sequences;
end.