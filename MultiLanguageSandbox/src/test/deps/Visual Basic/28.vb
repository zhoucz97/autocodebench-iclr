Module Module1
Function GenerateArithmeticExpressions(ByVal operations As List(Of Tuple(Of Char, Integer, Integer))) As List(Of String)
    Dim result As New List(Of String)
    
    For Each op In operations
        Dim operationChar As Char = op.Item1
        Dim operand1 As Integer = op.Item2
        Dim operand2 As Integer = op.Item3
        
        Dim expression As String = ""
        Dim resultValue As Integer = 0
        
        Select Case operationChar
            Case "a"c
                expression = $"{operand1}+{operand2}"
                resultValue = operand1 + operand2
            Case "b"c
                expression = $"{operand1}-{operand2}"
                resultValue = operand1 - operand2
            Case "c"c
                expression = $"{operand1}*{operand2}"
                resultValue = operand1 * operand2
            Case Else
                ' Handle unknown operations if needed
                Continue For
        End Select
        
        ' Add the expression with result
        result.Add($"{expression}={resultValue}")
        ' Add the length of the expression string
        result.Add(expression.Length.ToString())
    Next
    
    Return result
End Function

Sub Check(testFunction As Func(Of List(Of Tuple(Of Char, Integer, Integer)), List(Of String)))
Dim testOperations As New List(Of Tuple(Of Char, Integer, Integer)) From {
Tuple.Create("a"c, 15, 25),
Tuple.Create(" "c, 35, 15),
Tuple.Create("b"c, 50, 20),
Tuple.Create(" "c, 30, 10),
Tuple.Create("c"c, 4, 6)
}

Dim expectedResult As New List(Of String) From {
"15+25=40", "8", "35+15=50", "8", "50-20=30", "8", "30-10=20", "8", "4*6=24", "6"
}

Dim results = testFunction(testOperations)

For i As Integer = 0 To expectedResult.Count - 1
Debug.Assert(results(i) = expectedResult(i), $"Test case {i + 1} failed")
Next

Console.WriteLine("All test cases passed")
End Sub

Sub Main()
' Testing the function
Check(AddressOf GenerateArithmeticExpressions)
End Sub
End Module