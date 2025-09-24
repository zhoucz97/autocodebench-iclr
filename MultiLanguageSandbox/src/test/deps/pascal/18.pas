{$mode objfpc}{$H+}
uses
  SysUtils;
(*
Creates an ID by combining two words in a specific manner.
Parameters:
- word1 (string): The first word to be used in the ID.
- word2 (string): The second word to be used in the ID.
Returns:
string: A divine ID formed by reversing every other character of the second
        word and combining it with the first word.
Examples:
    >>> create_id('fish','cat')
     = 'ftiasch'
*)

function create_id(const word1, word2: AnsiString): AnsiString;
var
  i, len1, len2: Integer;
  processed_word2: AnsiString;
begin
  // Process word2: reverse it and then take every other character starting from the first
  processed_word2 := '';
  len2 := Length(word2);
  for i := len2 downto 1 do
  begin
    processed_word2 := processed_word2 + word2[i];
  end;
  
  // Now take every other character from the reversed word2
  len2 := Length(processed_word2);
  processed_word2 := '';
  for i := 1 to len2 do
  begin
    if (i mod 2) = 1 then
      processed_word2 := processed_word2 + word2[len2 - i + 1];
  end;
  
  // Now interleave characters from word1 and processed_word2
  Result := '';
  len1 := Length(word1);
  len2 := Length(processed_word2);
  i := 1;
  while (i <= len1) or (i <= len2) do
  begin
    if i <= len1 then
      Result := Result + word1[i];
    if i <= len2 then
      Result := Result + processed_word2[i];
    Inc(i);
  end;
end;
procedure TestCreateID;
var
  id: AnsiString;
begin
  id := create_id('fish', 'cat');
  Assert(id = 'ftiasch');

  id := create_id('icpc', 'acm');
  Assert(id = 'imccpac');

  id := create_id('oo', 'w');
  Assert(id = 'owo');

  // Add more test samples
  id := create_id('hello', 'world');
  Assert(id = 'hdellrloow');

  id := create_id('abc', 'def');
  Assert(id = 'afbecd');

  id := create_id('buaanb', 'nbbuaa');
  Assert(id = 'bauaauabnbbn');

  id := create_id('xtuisgood', 'ilovextu');
  Assert(id = 'xuttuxiesvgooloid');
end;

begin
  TestCreateID;
end.