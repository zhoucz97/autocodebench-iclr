Module Module1
Function PowerOfTwo(ByVal n As Integer) As Integer
    ' Check if n is within the valid range (0 <= n < 31)
    If n < 0 Or n >= 31 Then
        Throw New ArgumentException("n must be between 0 and 30 inclusive")
    End If
    
    ' Calculate 2^n using bit shifting (more efficient than Math.Pow)
    Return 1 << n
End Function

    Sub Main()
        ' Test the PowerOfTwo function
        Debug.Assert(PowerOfTwo(2) = 4)
        Debug.Assert(PowerOfTwo(5) = 32)
        Debug.Assert(PowerOfTwo(0) = 1)
        Debug.Assert(PowerOfTwo(10) = 1024)
        Debug.Assert(PowerOfTwo(15) = 32768)
        Debug.Assert(PowerOfTwo(30) = 1073741824)
    End Sub
End Module