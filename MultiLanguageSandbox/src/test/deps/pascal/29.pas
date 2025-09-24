{$ASSERTIONS ON}
(*
  Evaluate the grade for an input integer.
  Given an integer score, this function returns a character representing the grade.
  An 'A' grade is assigned for scores between 90 and 100 (inclusive),
  while a 'B' grade is given for all other scores.
  
  Example usage:
    >>> evaluate_integer_grade(90)
    A
*)

function evaluate_integer_grade(score: integer): char;
begin
  if (score >= 90) and (score <= 100) then
    evaluate_integer_grade := 'A'
  else
    evaluate_integer_grade := 'B';
end;
procedure testEvaluateIntegerGrade;
begin
  Assert(evaluate_integer_grade(90) = 'A');
  Assert(evaluate_integer_grade(89) = 'B');
  Assert(evaluate_integer_grade(95) = 'A');
  Assert(evaluate_integer_grade(100) = 'A');
  Assert(evaluate_integer_grade(101) = 'B');
end;

begin
  testEvaluateIntegerGrade;
end.