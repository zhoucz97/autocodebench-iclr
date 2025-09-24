{$ASSERTIONS ON}

uses
  SysUtils;
(*
  Function: minRoundsToSameChar
  Given a string consisting of lowercase English letters, this function
  calculates the minimum number of rounds needed to modify the string so
  that all of its characters are the same. In each round, you can change
  one character to another character.

  Examples:
    - minRoundsToSameChar('aab') -> 1
*)

function minRoundsToSameChar(s: AnsiString): Integer;
var
  freq: array['a'..'z'] of Integer;
  i, maxFreq: Integer;
begin
  // Initialize frequency array
  for i := Ord('a') to Ord('z') do
    freq[Chr(i)] := 0;

  // Count the frequency of each character in the string
  for i := 1 to Length(s) do
    Inc(freq[s[i]]);

  // Find the maximum frequency among all characters
  maxFreq := 0;
  for i := Ord('a') to Ord('z') do
  begin
    if freq[Chr(i)] > maxFreq then
      maxFreq := freq[Chr(i)];
  end;

  // The result is the total length minus the maximum frequency
  minRoundsToSameChar := Length(s) - maxFreq;
end;
procedure testMinRoundsToSameChar;
begin
  Assert(minRoundsToSameChar('aab') = 1);
  Assert(minRoundsToSameChar('abc') = 2);
  Assert(minRoundsToSameChar('aaa') = 0);
  Assert(minRoundsToSameChar('abab') = 1);
  Assert(minRoundsToSameChar('zzzzz') = 0);
end;

begin
  testMinRoundsToSameChar;
end.