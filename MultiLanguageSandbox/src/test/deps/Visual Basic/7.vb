Module Module1
Function MaxConsecutiveOnes(ByVal nums As List(Of Integer)) As Integer
    Dim maxCount As Integer = 0
    Dim currentCount As Integer = 0
    
    For Each num As Integer In nums
        If num = 1 Then
            currentCount += 1
            If currentCount > maxCount Then
                maxCount = currentCount
            End If
        Else
            currentCount = 0
        End If
    Next
    
    Return maxCount
End Function

Sub Check()
Debug.Assert(MaxConsecutiveOnes(New List(Of Integer) From {1, 1, 0, 1, 1, 1}) = 3)
Debug.Assert(MaxConsecutiveOnes(New List(Of Integer) From {1, 0, 1, 1, 0, 1}) = 2)
Debug.Assert(MaxConsecutiveOnes(New List(Of Integer) From {0, 0, 0}) = 0)
Debug.Assert(MaxConsecutiveOnes(New List(Of Integer) From {1, 1, 1, 1, 1}) = 5)
Debug.Assert(MaxConsecutiveOnes(New List(Of Integer) From {}) = 0)
Debug.Assert(MaxConsecutiveOnes(New List(Of Integer) From {0, 1, 1, 0, 1, 1, 1, 0, 1}) = 3)

Console.WriteLine("Pass")
End Sub

Sub Main()
' Test the function
Check()
End Sub
End Module