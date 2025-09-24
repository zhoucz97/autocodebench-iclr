Module Module1
Function MaxSatisfiedChildren(ByVal g As List(Of Integer), ByVal s As List(Of Integer)) As Integer
    ' Sort both lists in ascending order
    g.Sort()
    s.Sort()
    
    Dim childIndex As Integer = 0
    Dim cookieIndex As Integer = 0
    Dim satisfiedCount As Integer = 0
    
    ' Iterate through both lists to find matches
    While childIndex < g.Count AndAlso cookieIndex < s.Count
        If s(cookieIndex) >= g(childIndex) Then
            ' Cookie is big enough for this child
            satisfiedCount += 1
            childIndex += 1
            cookieIndex += 1
        Else
            ' Cookie is too small, try next cookie
            cookieIndex += 1
        End If
    End While
    
    Return satisfiedCount
End Function

Sub Check()
Debug.Assert(MaxSatisfiedChildren(New List(Of Integer) From {1, 2, 3}, New List(Of Integer) From {1, 1}) = 1)
Debug.Assert(MaxSatisfiedChildren(New List(Of Integer) From {1, 2}, New List(Of Integer) From {1, 2, 3}) = 2)
Debug.Assert(MaxSatisfiedChildren(New List(Of Integer) From {1, 2, 3}, New List(Of Integer) From {3}) = 1)
Debug.Assert(MaxSatisfiedChildren(New List(Of Integer) From {10, 9, 8, 7}, New List(Of Integer) From {5, 6, 7, 8}) = 2)
Debug.Assert(MaxSatisfiedChildren(New List(Of Integer) From {}, New List(Of Integer) From {1, 2, 3}) = 0)
Debug.Assert(MaxSatisfiedChildren(New List(Of Integer) From {1, 2, 3}, New List(Of Integer) From {}) = 0)

Console.WriteLine("Pass")
End Sub

Sub Main()
' Test the function
Check()
End Sub
End Module