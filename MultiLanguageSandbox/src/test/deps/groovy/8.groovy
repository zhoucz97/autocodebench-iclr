
"""
class MinStack:

    def __init__(self):


    def push(self, val: int) -> None:


    def pop(self) -> None:


    def top(self) -> int:


    def getMin(self) -> int:



# Your MinStack object will be instantiated and called as such:
# obj = MinStack()
# obj.push(val)
# obj.pop()
# param_3 = obj.top()
# param_4 = obj.getMin()
"""

        stack = new Stack<>()
        minStack = new Stack<>()
    }

    void push(int val) {
        stack.push(val)
        if (minStack.isEmpty() || val <= minStack.peek()) {
            minStack.push(val)
        }
    }

    void pop() {
        if (!stack.isEmpty()) {
            int popped = stack.pop()
            if (popped == minStack.peek()) {
                minStack.pop()
            }
        }
    }

    int top() {
        if (!stack.isEmpty()) {
            return stack.peek()
        }
        throw new IllegalStateException("Stack is empty")
    }

    int getMin() {
        if (!minStack.isEmpty()) {
            return minStack.peek()
        }
        throw new IllegalStateException("Stack is empty")
    }
MinStack minStack = new MinStack();
minStack.push(-2);
minStack.push(0);
minStack.push(-3);
assert minStack.getMin() == -3;
minStack.pop();
assert minStack.top() == 0;
assert minStack.getMin() == -2;

minStack = new MinStack();
minStack.push(1);
minStack.push(2);
minStack.push(3);
assert minStack.getMin() == 1;
minStack.pop();
assert minStack.top() == 2;
assert minStack.getMin() == 1;

minStack = new MinStack();
minStack.push(3);
minStack.push(2);
minStack.push(1);
assert minStack.getMin() == 1;
minStack.pop();
assert minStack.top() == 2;
assert minStack.getMin() == 2;