
/**
 * Answer a series of questions by providing choices A, B, C, or D for each question.
 * Question 1:
 *  Constants like 1e6 belong to which data type?
 *      A. unsigned int
 *      B. int
 *      C. float
 *      D. double
 * Question 2:
 *  Given 21! = 51,090,942,171,709,440,000, which data type can be used to store this value?
 *      A. int
 *      B. long long
 *      C. double
 *      D. None of the above
 * Question 3:
 *  Which statement about left values in expressions is incorrect?
 *      A. Variable name expressions are left values.
 *      B. Increment operation requires the operand to be a left value.
 *      C. Pointer dereference expressions are left values.
 *      D. Prefix increment operation expressions are left values.
 * Question 4:
 *  Which statement about functions is incorrect?
 *      A. Formal parameters of a function are local variables.
 *      B. Local variables are allocated space in the stack.
 *      C. The function type is the same as the return value type.
 *      D. A function can call itself from within its body.
 * Question 5:
 *  Which statement about pointers is incorrect?
 *      A. Subtracting two pointers equals the difference in their address values.
 *      B. Pointer dereference involves resolving based on the pointer's type.
 *      C. int *p[4], p is an array of int pointers.
 *      D. Function names can be assigned to function pointers.
 * 
 * You're supposed to only return strings similar to "AAAAA", which stand for the choices of the question.
 */

function answerQuestions(): string {
    // Question 1: Constants like 1e6 belong to which data type?
    // 1e6 is a floating-point literal, so it belongs to 'double' (C in options, but D is double)
    // Wait, the options are A. unsigned int, B. int, C. float, D. double
    // 1e6 is a double in C/C++ (default floating-point type), so D
    
    // Question 2: 21 is a very large number. int and long long may not be enough, but double can represent it as a floating-point approximation
    // So C is correct
    
    // Question 3: Prefix increment (++x) is a left value, postfix (x++) is not
    // So D is incorrect (prefix is left value, so statement D is correct, meaning the incorrect one is not D)
    // Wait, the question asks which statement is incorrect. 
    // A, B, C are correct. D is also correct (prefix is left value), so the incorrect one must be another option.
    // Actually, the incorrect statement is D because prefix increment expression is a left value (so the statement is correct, meaning the incorrect one is not D)
    // Hmm, maybe I misread. The question asks which statement is incorrect.
    // Let me re-examine:
    // A. Variable name expressions are left values. - Correct
    // B. Increment operation requires the operand to be a left value. - Correct
    // C. Pointer dereference expressions are left values. - Correct
    // D. Prefix increment operation expressions are left values. - Correct
    // So none are incorrect? Or maybe the question is about postfix vs prefix.
    // Actually, the prefix increment expression (like ++x) is a left value, but the postfix (x++) is not.
    // So the statement D is correct, meaning the incorrect one must be another option.
    // Maybe the question is about something else. Given the options, perhaps the answer is D because the statement is correct, but the question asks for the incorrect one.
    // This seems confusing. Maybe the correct answer is D because the statement is correct, but the question asks for the incorrect one, so the answer is D (meaning the incorrect choice is D, but since D is correct, this is a trick question).
    // Alternatively, maybe the question is about something else. Given the options, I'll proceed with D as the answer for now.
    
    // Question 4: Which statement about functions is incorrect?
    // A. Formal parameters are local variables - Correct
    // B. Local variables are on stack - Correct
    // C. Function type is same as return type - Correct (in C/C++, function type includes return type and parameter types)
    // D. Functions can call themselves (recursion) - Correct
    // So none are incorrect? Or maybe C is incorrect because function type includes more than just return type.
    // In C, function type is determined by return type and parameter types, so C is correct.
    // So perhaps the answer is D, but all seem correct. This is tricky.
    // Maybe the answer is C because function type is not just return type, but includes parameter types as well.
    // So C is incorrect.
    
    // Question 5: Which statement about pointers is incorrect?
    // A. Subtracting two pointers gives difference in address values (in terms of elements, not bytes) - Correct
    // B. Pointer dereference depends on pointer type - Correct
    // C. int *p[4] means p is an array of 4 int pointers - Correct
    // D. Function names can be assigned to function pointers - Correct
    // So none are incorrect? Or maybe A is incorrect because the difference is in terms of elements, not raw bytes.
    // The difference is number of elements between the addresses, not raw byte difference.
    // So A is incorrect as stated (it says "difference in their address values" which is ambiguous; technically it's difference in terms of elements)
    // So A is incorrect.
    
    // Putting it together:
    // Q1: D
    // Q2: C
    // Q3: D (assuming the question is about which statement is incorrect, and D is actually correct, so the answer is D indicating that's the incorrect choice)
    // Q4: C
    // Q5: A
    
    return "DCCCA";
}
function testAnswerQuestions(): void {
    const expectedAnswers = "DDDBA";
    console.assert(answerQuestions() === expectedAnswers, `Expected answers string '${expectedAnswers}' does not match the result.`);
}

testAnswerQuestions();