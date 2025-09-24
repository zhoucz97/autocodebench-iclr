program QuickSortProgram;

uses
  SysUtils;

type
  TIntArray = array of Integer;

function Quicksort(list: TIntArray): TIntArray;
var
  pivot: Integer;
  rest, less, greater: TIntArray;
  i, lessCount, greaterCount: Integer;
begin
  if Length(list) <= 1 then
  begin
    Result := list;
    Exit;
  end;

  pivot := list[0];
  SetLength(rest, Length(list) - 1);
  for i := 1 to High(list) do
    rest[i - 1] := list[i];

  lessCount := 0;
  greaterCount := 0;
  for i := 0 to High(rest) do
  begin
    if rest[i] < pivot then
      Inc(lessCount)
    else
      Inc(greaterCount);
  end;

  SetLength(less, lessCount);
  SetLength(greater, greaterCount);
  lessCount := 0;
  greaterCount := 0;

  for i := 0 to High(rest) do
  begin
    if rest[i] < pivot then
    begin
      less[lessCount] := rest[i];
      Inc(lessCount);
    end
    else
    begin
      greater[greaterCount] := rest[i];
      Inc(greaterCount);
    end;
  end;

  less := Quicksort(less);
  greater := Quicksort(greater);

  SetLength(Result, Length(less) + 1 + Length(greater));
  for i := 0 to High(less) do
    Result[i] := less[i];
  Result[Length(less)] := pivot;
  for i := 0 to High(greater) do
    Result[Length(less) + 1 + i] := greater[i];
end;

function ArraysEqual(const A, B: TIntArray): Boolean;
var
  i: Integer;
begin
  Result := Length(A) = Length(B);
  if Result then
    for i := 0 to High(A) do
      if A[i] <> B[i] then
      begin
        Result := False;
        Break;
      end;
end;

procedure CheckQuicksort;
var
  test1, test2, test3, test4: TIntArray;
  expected1, expected2, expected3, expected4: TIntArray;
begin
  test1 := TIntArray.Create(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5);
  expected1 := TIntArray.Create(1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 9);
  Assert(ArraysEqual(Quicksort(test1), expected1));

  test2 := TIntArray.Create(5, 4, 3, 2, 1);
  expected2 := TIntArray.Create(1, 2, 3, 4, 5);
  Assert(ArraysEqual(Quicksort(test2), expected2));

  SetLength(test3, 0);
  SetLength(expected3, 0);
  Assert(ArraysEqual(Quicksort(test3), expected3));

  test4 := TIntArray.Create(1);
  expected4 := TIntArray.Create(1);
  Assert(ArraysEqual(Quicksort(test4), expected4));
end;

begin
  CheckQuicksort;
end.
