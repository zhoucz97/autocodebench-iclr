import java.util.*
class ParenthesesRemover {
    static List<String> removeInvalidParentheses(String s) {
        """
        Remove the minimum number of invalid parentheses in order to make the input string valid.
        A string is considered valid if it has the same number of open and close parentheses.
        >>> removeInvalidParentheses("()())()")
        ["(())()","()()()"]
        >>> removeInvalidParentheses("(a)())()")
        ["(a())()","(a)()()"]
        >>> removeInvalidParentheses(")(")
        [""]
        """

    List<String> result = []
    Set<String> visited = new HashSet<>()
    Queue<String> queue = new LinkedList<>()
    boolean found = false
    
    queue.add(s)
    visited.add(s)
    
    while (!queue.isEmpty()) {
        String current = queue.poll()
        
        if (isValid(current)) {
            result.add(current)
            found = true
        }
        
        if (found) continue
        
        for (int i = 0; i < current.length(); i++) {
            if (current.charAt(i) != '(' && current.charAt(i) != ')') continue
            
            String next = current.substring(0, i) + current.substring(i + 1)
            
            if (!visited.contains(next)) {
                visited.add(next)
                queue.add(next)
            }
        }
    }
    
    if (result.isEmpty()) {
        return [""]
    }
    
    return result
}

static boolean isValid(String s) {
    int balance = 0
    for (char c : s.toCharArray()) {
        if (c == '(') {
            balance++
        } else if (c == ')') {
            balance--
            if (balance < 0) {
                return false
            }
        }
    }
    return balance == 0
}
}
def parenthesesRemover = new ParenthesesRemover()
assert parenthesesRemover.removeInvalidParentheses("()())()") == ["(())()","()()()"]
assert parenthesesRemover.removeInvalidParentheses("(a)())()") == ["(a())()","(a)()()"]
assert parenthesesRemover.removeInvalidParentheses(")(") == [""]
assert parenthesesRemover.removeInvalidParentheses("()") == ["()"]
assert parenthesesRemover.removeInvalidParentheses(")") == [""]
assert parenthesesRemover.removeInvalidParentheses("(") == [""]
assert parenthesesRemover.removeInvalidParentheses("") == [""]