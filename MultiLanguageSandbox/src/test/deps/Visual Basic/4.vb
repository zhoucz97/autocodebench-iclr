Module Module1
Function FindMissingNumbers(ByVal nums As List(Of Integer)) As List(Of Integer)
    Dim n As Integer = nums.Count
    Dim missingNumbers As New List(Of Integer)()
    
    ' Mark numbers by negating the value at their index (1-based to 0-based)
    For i As Integer = 0 To n - 1
        Dim num As Integer = Math.Abs(nums(i))
        If num <= n Then
            nums(num - 1) = -Math.Abs(nums(num - 1))
        End If
    Next
    
    ' Collect indices where the value is still positive (1-based to 0-based)
    For i As Integer = 0 To n - 1
        If nums(i) > 0 Then
            missingNumbers.Add(i + 1)
        End If
    Next
    
    Return missingNumbers
End Function

Sub Check()
Debug.Assert(FindMissingNumbers(New List(Of Integer) From {4, 3, 2, 7, 8, 2, 3, 1}).SequenceEqual(New List(Of Integer) From {5, 6}))
Debug.Assert(FindMissingNumbers(New List(Of Integer) From {1, 1}).SequenceEqual(New List(Of Integer) From {2}))
Debug.Assert(FindMissingNumbers(New List(Of Integer) From {1, 2, 3, 4, 5}).Count = 0)
Debug.Assert(FindMissingNumbers(New List(Of Integer) From {7, 8, 9, 11, 12}).SequenceEqual(New List(Of Integer) From {1, 2, 3, 4, 5}))
Debug.Assert(FindMissingNumbers(New List(Of Integer) From {}).Count = 0)
Debug.Assert(FindMissingNumbers(New List(Of Integer) From {2, 2, 2}).SequenceEqual(New List(Of Integer) From {1, 3}))

Console.WriteLine("Pass")
End Sub

Sub Main()
' Test the function
Check()
End Sub
End Module