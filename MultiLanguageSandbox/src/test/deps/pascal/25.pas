{$ASSERTIONS ON}

uses
  SysUtils;
(*
  Shifts all characters by 5 positions in alphabetical order. 
  Only letters are replaced, and all letters are in uppercase.
  
  Parameters:
  text (str): The input text to be processed.
  
  Returns:
  str: The transformed text with characters shifted by 5 positions.
  >>> shift_characters('NS BFW, JAJSYX TK NRUTWYFSHJ FWJ YMJ WJXZQY TK YWNANFQ HFZXJX')
  'IN WAR, EVENTS OF IMPORTANCE ARE THE RESULT OF TRIVIAL CAUSES'
*)

function shift_characters(text: ansistring): ansistring;
var
  i, pos: integer;
  c: char;
begin
  Result := '';
  for i := 1 to Length(text) do
  begin
    c := text[i];
    if (c >= 'A') and (c <= 'Z') then
    begin
      pos := Ord(c) - Ord('A');
      pos := (pos + 5) mod 26;
      c := Chr(pos + Ord('A'));
    end;
    Result := Result + c;
  end;
end;
procedure testShiftCharacters;
var
  test1, test2, test3: ansistring;
begin
  test1 := 'NS BFW, JAJSYX TK NRUTWYFSHJ FWJ YMJ WJXZQY TK YWNANFQ HFZXJX';
  test2 := 'N BTZQI WFYMJW GJ KNWXY NS F QNYYQJ NGJWNFS ANQQFLJ YMFS XJHTSI NS WTRJ';
  test3 := 'IFSLJW PSTBX KZQQ BJQQ YMFY HFJXFW NX RTWJ IFSLJWTZX YMFS MJ';

  Assert(shift_characters(test1) = 'IN WAR, EVENTS OF IMPORTANCE ARE THE RESULT OF TRIVIAL CAUSES');
  Assert(shift_characters(test2) = 'I WOULD RATHER BE FIRST IN A LITTLE IBERIAN VILLAGE THAN SECOND IN ROME');
  Assert(shift_characters(test3) = 'DANGER KNOWS FULL WELL THAT CAESAR IS MORE DANGEROUS THAN HE');
end;

begin
  testShiftCharacters;
end.