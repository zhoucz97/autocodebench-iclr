Module Module1
Function MaxSumOfMinPairs(ByVal nums As List(Of Integer)) As Integer
    nums.Sort()
    Dim sum As Integer = 0
    For i As Integer = 0 To nums.Count - 1 Step 2
        sum += nums(i)
    Next
    Return sum
End Function

Sub Check()
Debug.Assert(MaxSumOfMinPairs(New List(Of Integer) From {1, 4, 3, 2}) = 4)
Debug.Assert(MaxSumOfMinPairs(New List(Of Integer) From {6, 2, 6, 5, 1, 2}) = 9)
Debug.Assert(MaxSumOfMinPairs(New List(Of Integer) From {1, 2, 3, 4, 5, 6}) = 9)
Debug.Assert(MaxSumOfMinPairs(New List(Of Integer) From {}) = 0)
Debug.Assert(MaxSumOfMinPairs(New List(Of Integer) From {7, 3, 1, 0, 0, 6}) = 7)

Console.WriteLine("Pass")
End Sub

Sub Main()
' Test the function
Check()
End Sub
End Module