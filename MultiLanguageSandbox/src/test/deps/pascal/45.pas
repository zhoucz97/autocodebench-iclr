{$ASSERTIONS ON}
(*
   Given a 1 by n pixel image where each pixel has a color represented by an integer,
   you can pick a color and change all connected pixels of the same color to the chosen color.
   Two pixels are connected if they are adjacent and have the same color.
   Find the minimum number of operations required to make all pixels the same color.

   Note: For each color, there are at most 20 pixels of that color.

   Examples:
       >>> minOperations(5, [1, 2, 3, 2, 1])
       2
*)

function minOperations(n: Integer; pixels: array of Integer): Integer;
var
  i, j, minOps, currentOps, color: Integer;
  used: array of Boolean;
  colors: set of Integer;
begin
  // Collect all unique colors in the pixels array
  colors := [];
  for i := 0 to n - 1 do
    Include(colors, pixels[i]);
  
  minOps := MaxInt;
  
  // For each unique color, calculate the operations needed to make all pixels this color
  for color in colors do
  begin
    currentOps := 0;
    i := 0;
    while i < n do
    begin
      if pixels[i] <> color then
      begin
        // Found a connected component of a different color
        currentOps := currentOps + 1;
        // Skip all adjacent pixels of the same color
        while (i < n) and (pixels[i] = pixels[i - 1]) do
          Inc(i);
      end
      else
      begin
        Inc(i);
      end;
    end;
    if currentOps < minOps then
      minOps := currentOps;
  end;
  
  Result := minOps;
end;
procedure testMinOperations;
begin
  Assert(minOperations(5, [1, 2, 3, 2, 1]) = 2);
  Assert(minOperations(4, [1, 1, 2, 2]) = 1);
  Assert(minOperations(5, [1, 2, 1, 4, 2]) = 3);
  Assert(minOperations(5, [5, 5, 5, 5, 5]) = 0);
  Assert(minOperations(6, [1, 1, 1, 2, 2, 2]) = 1);
  Assert(minOperations(7, [1, 3, 3, 3, 2, 2, 2]) = 2);
  Assert(minOperations(8, [4, 4, 4, 4, 3, 3, 3, 3]) = 1);
  Assert(minOperations(9, [1, 2, 3, 4, 5, 6, 7, 8, 9]) = 8);
  Assert(minOperations(10, [1, 2, 1, 2, 1, 2, 1, 2, 1, 2]) = 5);
  Assert(minOperations(3, [3, 3, 3]) = 0);
  Assert(minOperations(4, [2, 1, 1, 2]) = 1);
end;

begin
  testMinOperations;
end.