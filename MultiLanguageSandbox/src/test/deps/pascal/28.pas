{$ASSERTIONS ON}
(*
  Display the ASCII information for a given character.
    Parameters:
    - character: The input character for which ASCII information is to be displayed.

    Returns:
    - The corresponding ASCII code as an integer.
      Example usage:
      >>> display_ascii_info('A')
      65
*)

function display_ascii_info(character: char): integer;
begin
  // Display the ASCII information (character and its code)
  writeln('Character: ', character, ' | ASCII Code: ', ord(character));
  
  // Return the ASCII code as an integer
  display_ascii_info := ord(character);
end;
procedure testDisplayAsciiInfo;
begin
  Assert(display_ascii_info('A') = 65);
  Assert(display_ascii_info('B') = 66); // Additional test case
  Assert(display_ascii_info('0') = 48); // Additional test case
  Assert(display_ascii_info(' ') = 32); // Additional test case
end;

begin
  testDisplayAsciiInfo;
  // WriteLn('All tests passed.');
end.