Module Module1
Function MultiplyNumbers(ByVal A As Integer, ByVal B As Integer) As Integer
    ' Check if inputs are within the valid range (1 to 50000)
    If A < 1 Or A > 50000 Or B < 1 Or B > 50000 Then
        Throw New ArgumentException("Both numbers must be between 1 and 50000")
    End If
    
    ' Calculate and return the product
    Return A * B
End Function

    Sub Main()
        ' Test the MultiplyNumbers function
        Debug.Assert(MultiplyNumbers(2, 3) = 6)
        Debug.Assert(MultiplyNumbers(10, 20) = 200)
        Debug.Assert(MultiplyNumbers(1, 50000) = 50000)
        Debug.Assert(MultiplyNumbers(100, 200) = 20000)
        Debug.Assert(MultiplyNumbers(123, 456) = 56088)
        Debug.Assert(MultiplyNumbers(50000, 1) = 50000)
        Debug.Assert(MultiplyNumbers(25000, 2) = 50000)
    End Sub
End Module