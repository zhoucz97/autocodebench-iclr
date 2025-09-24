Module Module1
Function CalculateExpression(a As Integer, b As Integer, c As Integer) As Integer
    ' Check if the input values are within the specified range
    If (-10000 < a And a < 10000) And (-10000 < b And b < 10000) And (-10000 < c And c < 10000) Then
        ' Calculate the expression (a + b) * c
        CalculateExpression = (a + b) * c
    Else
        ' Return an error message if the input is out of range
        MsgBox "Input values must be between -10,000 and 10,000.", vbExclamation, "Input Range Error"
        CalculateExpression = -1 ' Or any error code or message you prefer
    End If
End Function

    Sub Main()
        ' Test the CalculateExpression function
        Debug.Assert(CalculateExpression(2, 3, 5) = 25)
        Debug.Assert(CalculateExpression(-1, 2, 3) = 3)
        Debug.Assert(CalculateExpression(0, 0, 1) = 0)
        Debug.Assert(CalculateExpression(10, -5, 2) = 10)
        Debug.Assert(CalculateExpression(-2, -3, -4) = 20)
        Debug.Assert(CalculateExpression(1000, 2000, 3) = 9000)
        Debug.Assert(CalculateExpression(-100, 50, 10) = -500)
    End Sub
End Module