{$ASSERTIONS ON}

uses
  Math, SysUtils;
(*
    Calculates the value of the function for a given input.
      Parameters:
        - x (integer): Input value for the function.
      Returns:
        - A string with either the calculated function value rounded to 5 decimal places,
          or a notice that the input is not in the defined domain ("Not define").
       
      Function Definitions:
        - For 0 <= x < 10: y = cos(x + 3.0)
        - For 10 <= x < 20: y = (cos(x + 7.5))^2
        - For 20 <= x < 30: y = (cos(x + 4.0))^4
        >>> calculate_function_value(40)
        'Not define'
*)

function calculate_function_value(x: Integer): string;
var
  y: Double;
begin
  if (x >= 0) and (x < 10) then
    y := cos(x + 3.0)
  else if (x >= 10) and (x < 20) then
    y := power(cos(x + 7.5), 2)
  else if (x >= 20) and (x < 30) then
    y := power(cos(x + 4.0), 4)
  else
  begin
    Result := 'Not define';
    Exit;
  end;
  
  // Round to 5 decimal places and convert to string
  Result := FloatToStrF(y, ffFixed, 10, 5);
end;
procedure testCalculateFunctionValue;
var
  result: string;
begin
  result := calculate_function_value(40);
  Assert(result = 'Not define');
  // WriteLn(calculate_function_value(5))

  // Additional test cases based on provided function definitions
  Assert(calculate_function_value(5) = '-0.14550');
  Assert(calculate_function_value(15) = '0.76266');
  Assert(calculate_function_value(25) = '0.31314');
  Assert(calculate_function_value(-1) = 'Not define');
end;

begin
  testCalculateFunctionValue;
end.