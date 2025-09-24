Module Module1
Function ArithmeticSequenceNthTerm(ByVal a1 As Integer, ByVal a2 As Integer, ByVal n As Integer) As Integer
    ' Calculate the common difference d
    Dim d As Integer
    d = a2 - a1
    
    ' Calculate the nth term using the formula: a1 + (n-1)*d
    Dim nthTerm As Integer
    nthTerm = a1 + (n - 1) * d
    
    Return nthTerm
End Function

    Sub Main()
        ' Test the ArithmeticSequenceNthTerm function
        Debug.Assert(ArithmeticSequenceNthTerm(1, 4, 100) = 298)
        Debug.Assert(ArithmeticSequenceNthTerm(5, 7, 3) = 9)
        Debug.Assert(ArithmeticSequenceNthTerm(-3, -1, 5) = 5)
        Debug.Assert(ArithmeticSequenceNthTerm(10, 20, 10) = 100)
        Debug.Assert(ArithmeticSequenceNthTerm(0, 3, 4) = 9)
        Debug.Assert(ArithmeticSequenceNthTerm(-5, -2, 20) = 52)
        Debug.Assert(ArithmeticSequenceNthTerm(2, 5, 50) = 149)
    End Sub
End Module