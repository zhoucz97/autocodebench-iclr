{$ASSERTIONS ON}

uses
  Math, SysUtils;
(* 
Given two positive integers n and k, find the number of positive integers x,
where 1 <= x <= n, that can be expressed as x = a^b with a and b being positive
integers and b >= k. Different legal representations of the same number are counted
only once.
    >>> countPowerNumbers(99, 1)
    99
*)

function countPowerNumbers(n, k: integer): integer;
var
  powerSet: set of integer;
  a, b, currentPower: integer;
begin
  powerSet := [];
  
  // The maximum exponent b can be such that 2^b <= n
  b := k;
  while true do
  begin
    if Power(2, b) > n then
      Break;
    b := b + 1;
  end;
  b := b - 1; // Now b is the maximum exponent where 2^b <= n
  
  for b := k to b do
  begin
    a := 1;
    repeat
      currentPower := Trunc(Power(a, b));
      if currentPower > n then
        Break;
      Include(powerSet, currentPower);
      a := a + 1;
    until False;
  end;
  
  countPowerNumbers := Length(powerSet);
end;
procedure testCountPowerNumbers;
begin
  Assert(countPowerNumbers(99, 1) = 99);
  Assert(countPowerNumbers(99, 3) = 7);
  Assert(countPowerNumbers(99, 2) = 12);
  Assert(countPowerNumbers(10, 1) = 10);
  Assert(countPowerNumbers(10, 2) = 4);
  Assert(countPowerNumbers(500, 1) = 500);
  Assert(countPowerNumbers(500, 2) = 30);
  Assert(countPowerNumbers(500, 3) = 13);
  Assert(countPowerNumbers(1000, 1) = 1000);
  Assert(countPowerNumbers(1000, 2) = 41);
  Assert(countPowerNumbers(1000, 3) = 17);
  Assert(countPowerNumbers(1000, 93) = 1);
  Assert(countPowerNumbers(50, 2) = 10);
  Assert(countPowerNumbers(50, 3) = 5);
  Assert(countPowerNumbers(2, 3) = 1);
end;

begin
  testCountPowerNumbers;
end.