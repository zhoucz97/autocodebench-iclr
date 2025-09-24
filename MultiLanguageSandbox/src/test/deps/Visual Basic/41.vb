Module Module1
Function AbsoluteValue(ByVal n As Integer) As Integer
    If n < 0 Then
        Return -n
    Else
        Return n
    End If
End Function

    Sub Main()
        ' Test the AbsoluteValue function
        Debug.Assert(AbsoluteValue(-5) = 5)
        Debug.Assert(AbsoluteValue(3) = 3)
        Debug.Assert(AbsoluteValue(0) = 0)
        Debug.Assert(AbsoluteValue(-100) = 100)
        Debug.Assert(AbsoluteValue(50) = 50)
        Debug.Assert(AbsoluteValue(-9999) = 9999)
        Debug.Assert(AbsoluteValue(9999) = 9999)

    End Sub
End Module