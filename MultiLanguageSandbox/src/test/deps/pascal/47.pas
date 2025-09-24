{$ASSERTIONS ON}

uses
  SysUtils, Character;
(*
  Determine if a given string is a palindrome. A palindrome is a word, phrase,
  number, or other sequence of characters that reads the same forward and backward
  (ignoring spaces, punctuation, and capitalization).
      >>> isPalindrome('hello')
      false
*)

function isPalindrome(str: String): Boolean;
var
  i, j: Integer;
  cleanedStr: String;
begin
  // Create a cleaned version of the string (lowercase, no non-alphanumeric chars)
  cleanedStr := '';
  for i := 1 to Length(str) do
  begin
    if (str[i] in ['a'..'z', 'A'..'Z', '0'..'9']) then
    begin
      // Convert to lowercase
      if str[i] in ['A'..'Z'] then
        cleanedStr := cleanedStr + Chr(Ord(str[i]) + 32)
      else
        cleanedStr := cleanedStr + str[i];
    end;
  end;

  // Check if the cleaned string is a palindrome
  i := 1;
  j := Length(cleanedStr);
  while (i < j) and (cleanedStr[i] = cleanedStr[j]) do
  begin
    Inc(i);
    Dec(j);
  end;

  isPalindrome := (i >= j);
end;
procedure testIsPalindrome;
begin
  Assert(isPalindrome('A man a plan a canal Panama') = True);
  Assert(isPalindrome('No lemon, no melon') = True);
  Assert(isPalindrome('Was it a car or a cat I saw') = True);
  Assert(isPalindrome('Madam, in Eden, I''m Adam') = True);
  Assert(isPalindrome('Never odd or even') = True);
  Assert(isPalindrome('Eva, can I see bees in a cave') = True);
  Assert(isPalindrome('hello') = False);
  Assert(isPalindrome('GitHub') = False);
  Assert(isPalindrome('programming') = False);
end;

begin
  testIsPalindrome;
end.