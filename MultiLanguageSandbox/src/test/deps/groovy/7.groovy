
class ReversePolishNotationCalculator {
    static int calculate(List<String> tokens) {
        """
        Calculate the result of an arithmetic expression in Reverse Polish Notation.
        Reverse Polish notation (RPN) is a mathematical notation in which every operator follows all of its operands.

        Example:
        >>> calculate(["2", "1", "+", "3", "*"])
        9
        Explanation: ((2 + 1) * 3) = 9

        >>> calculate(["4", "13", "5", "/", "+"])
        6
        Explanation: (4 + (13 / 5)) = 6

        >>> calculate(["10", "6", "9", "3", "+", "-11", "*", "/", "*", "17", "+", "5", "+"])
        22
        Explanation: ((10 * (6 / ((9 + 3) * -11))) + 17) + 5 = 22
        """

    def stack = []
    
    tokens.each { token ->
        if (token in ['+', '-', '*', '/']) {
            // It's an operator, pop two operands from the stack
            def b = stack.pop() as int
            def a = stack.pop() as int
            
            switch (token) {
                case '+': stack.push(a + b); break
                case '-': stack.push(a - b); break
                case '*': stack.push(a * b); break
                case '/': 
                    // Integer division in Groovy rounds towards zero
                    stack.push((int)(a / b)); 
                    break
            }
        } else {
            // It's a number, push to stack
            stack.push(token as int)
        }
    }
    
    return stack.pop()
}
}
def calculator = new ReversePolishNotationCalculator()
assert calculator.calculate(["2", "1", "+", "3", "*"]) == 9
assert calculator.calculate(["4", "13", "5", "/", "+"]) == 6
assert calculator.calculate(["10", "6", "9", "3", "+", "-11", "*", "/", "*", "17", "+", "5", "+"]) == 22
assert calculator.calculate(["3", "4", "+", "2", "*"]) == 14
assert calculator.calculate(["15", "7", "1", "1", "+", "-", "/", "3", "*", "2", "1", "1", "+", "+", "-"]) == 5
assert calculator.calculate(["1", "2", "+", "3", "*"]) == 9
assert calculator.calculate(["10", "6", "9", "3", "-", "11", "*", "/", "*", "17", "+", "5", "+"]) == 22