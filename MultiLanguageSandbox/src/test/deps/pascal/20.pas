{$ASSERTIONS ON}

uses
  SysUtils;
(*
    Answer a series of questions by providing choices A, B, C, or D for each question.

    Question 1:
    Constants like 1e6 belong to which data type?
        A. unsigned int
        B. int
        C. float
        D. double

    Question 2:
    Given 21! = 51,090,942,171,709,440,000, which data type can be used to store this value?
        A. int
        B. long long (In Pascal, use Int64 for long integers)
        C. double
        D. None of the above

    Question 3:
    Which statement about left values in expressions is incorrect?
        A. Variable name expressions are left values.
        B. Increment operation requires the operand to be a left value.
        C. Pointer dereference expressions are left values.
        D. Prefix increment operation expressions are left values.

    Question 4:
    Which statement about functions is incorrect?
        A. Formal parameters of a function are local variables.
        B. Local variables are allocated space in the stack.
        C. The function type is the same as the return value type.
        D. A function can call itself from within its body.

    Question 5:
    Which statement about pointers is incorrect?
        A. Subtracting two pointers equals the difference in their address values.
        B. Pointer dereference involves resolving based on the pointer's type.
        C. int *p[4], p is an array of int pointers.
        D. Function names can be assigned to function pointers.

    You're supposed to only return strings similar to "AAAAA", which stand for the choices of the question.
*)

function answer_questions: string;
begin
  // Question 1: Constants like 1e6 belong to which data type?
  // 1e6 is a floating-point literal, so it belongs to 'double' in Pascal
  // Answer: D
  
  // Question 2: Given 21 = 51,090,942,171,709,440,000, which data type can be used to store this value?
  // 21 is a very large integer that exceeds the range of standard integer types
  // In Pascal, Int64 can hold up to 9,223,372,036,854,775,807 (for signed) or larger for unsigned
  // Answer: B (Int64)
  
  // Question 3: Which statement about left values in expressions is incorrect?
  // D is incorrect because prefix increment (e.g., ++x) returns the new value, not a left value
  // Answer: D
  
  // Question 4: Which statement about functions is incorrect?
  // C is incorrect because the function type includes the return type plus parameter types
  // Answer: C
  
  // Question 5: Which statement about pointers is incorrect?
  // A is incorrect because subtracting two pointers gives the difference in elements, not raw address values
  // Answer: A
  
  answer_questions := 'DCDCA';
end;
procedure testAnswerQuestions;
begin
    Assert(answer_questions = 'DDDBA', 'The answer string should match DDDBA');
end;

begin
    testAnswerQuestions;
end.