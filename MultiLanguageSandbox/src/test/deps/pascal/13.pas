{$ASSERTIONS ON}

uses
  SysUtils, Math;
(*
  Calculate the total score for a student based on the scores in different subjects.
  The function takes five integer arguments, each representing the score in one subject,
  and returns the total score by adding them together.
  Example:
    >>> calculate_total_score(100, 100, 100, 100, 100)
    500
*)

function calculate_total_score(score1, score2, score3, score4, score5: integer): integer;
begin
  calculate_total_score := score1 + score2 + score3 + score4 + score5;
end;
procedure testCalculateTotalScore;
begin
    Assert(calculate_total_score(100, 100, 100, 100, 100) = 500);
    Assert(calculate_total_score(0, 0, 0, 0, 0) = 0);
    Assert(calculate_total_score(20, 30, 40, 10, 50) = 150);
    Assert(calculate_total_score(23, 45, 67, 89, 12) = 236);
    Assert(calculate_total_score(5, 5, 5, 5, 5) = 25);
end;

begin
    testCalculateTotalScore;
end.