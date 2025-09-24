Module Module1
Function DetermineSign(ByVal N As Integer) As String
    If N > 0 Then
        Return "positive"
    ElseIf N < 0 Then
        Return "negative"
    Else
        Return "zero"
    End If
End Function


    Sub Main()
        ' Test the DetermineSign function
        Debug.Assert(DetermineSign(10) = "positive")
        Debug.Assert(DetermineSign(0) = "zero")
        Debug.Assert(DetermineSign(-5) = "negative")
        Debug.Assert(DetermineSign(1000000000) = "positive")
        Debug.Assert(DetermineSign(-1000000000) = "negative")
        Debug.Assert(DetermineSign(1) = "positive")
        Debug.Assert(DetermineSign(-1) = "negative")
    End Sub
End Module