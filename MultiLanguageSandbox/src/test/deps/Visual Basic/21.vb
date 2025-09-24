Module Module1
Function FindBallPosition(ByVal operations As String) As Integer
    Dim position As Integer = 1 ' Initial position of the ball is 1 (left cup)
    
    For Each op As Char In operations
        Select Case op
            Case "A"c
                ' Swap positions 1 and 2
                If position = 1 Then
                    position = 2
                ElseIf position = 2 Then
                    position = 1
                End If
            Case "B"c
                ' Swap positions 2 and 3
                If position = 2 Then
                    position = 3
                ElseIf position = 3 Then
                    position = 2
                End If
            Case "C"c
                ' Swap positions 1 and 3
                If position = 1 Then
                    position = 3
                ElseIf position = 3 Then
                    position = 1
                End If
        End Select
    Next
    
    Return position
End Function

Sub Check(ByVal testFunction As Func(Of String, Integer))
Debug.Assert(testFunction("A") = 2)
Debug.Assert(testFunction("BC") = 3)
Debug.Assert(testFunction("AB") = 3)
Debug.Assert(testFunction("C") = 3)
Debug.Assert(testFunction("BAC") = 2)
Debug.Assert(testFunction("CBA") = 1)
Debug.Assert(testFunction("ABCACBA") = 3)

Console.WriteLine("All test cases passed")
End Sub
Sub Main()
' Testing the function
Check(AddressOf FindBallPosition)
End Sub
End Module