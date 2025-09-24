Module Module1
Function ReverseInteger(ByVal number As Integer) As Integer
    Dim reversed As Integer = 0
    Dim num As Integer = Math.Abs(number) ' Work with absolute value first
    
    While num > 0
        Dim digit As Integer = num Mod 10
        reversed = reversed * 10 + digit
        num \= 10
    End While
    
    ' Restore the original sign
    If number < 0 Then
        reversed = -reversed
    End If
    
    Return reversed
End Function

    Sub CheckReverse(ByVal testFunction As Func(Of Integer, Integer))
        ' Test cases
        Debug.Assert(testFunction(123) = 321)
        Debug.Assert(testFunction(-123) = -321)
        Debug.Assert(testFunction(1000) = 1)
        Debug.Assert(testFunction(0) = 0)
        Debug.Assert(testFunction(-5050) = -505)

        Console.WriteLine("ReverseInteger function passed all tests")
    End Sub

    Sub Main()
        ' Testing the ReverseInteger function
        CheckReverse(AddressOf ReverseInteger)
    End Sub
End Module