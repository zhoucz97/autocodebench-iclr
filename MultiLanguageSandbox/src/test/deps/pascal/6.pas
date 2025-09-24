{$ASSERTIONS ON}
{
  Calculates the factorial of a given number N modulo 10007.
  
  Parameters:
  - N (integer): An integer representing the input value (N <= 10000).
  
  Returns:
  - integer: The factorial of N modulo 10007.
  
  Examples:
  - process_request(1) returns 1
  - process_request(2) returns 2
  - process_request(3) returns 6
}

function process_request(n: integer): integer;
var
  i, result: integer;
begin
  if n < 0 then
    process_request := 0  // Factorial is not defined for negative numbers
  else
  begin
    result := 1;
    for i := 1 to n do
    begin
      result := (result * i) mod 10007;
    end;
    process_request := result;
  end;
end;
procedure testProcessRequest;
begin
  Assert(process_request(0) = 1, 'Test for N = 0 failed');
  Assert(process_request(1) = 1, 'Test for N = 1 failed');
  Assert(process_request(2) = 2, 'Test for N = 2 failed');
  Assert(process_request(3) = 6, 'Test for N = 3 failed');
  Assert(process_request(4) = 24, 'Test for N = 4 failed');
  Assert(process_request(10) = 6266, 'Test for N = 10 failed');
  Assert(process_request(10000) = 6991, 'Test for N = 10000 failed');
end;

begin
  (* Run the tests *)
  testProcessRequest;
  // WriteLn('All tests passed.');
end.