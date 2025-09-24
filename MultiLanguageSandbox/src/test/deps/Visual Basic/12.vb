Module Module1
Function MaxCandyTypes(ByVal candyType As List(Of Integer)) As Integer
    ' Calculate the maximum number of candies Alice can eat (half of total)
    Dim maxEat As Integer = candyType.Count \ 2
    
    ' Get the distinct candy types
    Dim distinctTypes As New HashSet(Of Integer)(candyType)
    
    ' The result is the minimum between distinct types and maxEat
    Return Math.Min(distinctTypes.Count, maxEat)
End Function

Sub Check()
Debug.Assert(MaxCandyTypes(New List(Of Integer) From {1, 1, 2, 2, 3, 3}) = 3)
Debug.Assert(MaxCandyTypes(New List(Of Integer) From {1, 1, 2, 3}) = 2)
Debug.Assert(MaxCandyTypes(New List(Of Integer) From {6, 6, 6, 6}) = 1)
Debug.Assert(MaxCandyTypes(New List(Of Integer) From {}) = 0)
Debug.Assert(MaxCandyTypes(New List(Of Integer) From {1, 2, 3, 4, 5, 6, 7, 8}) = 4)

Console.WriteLine("Pass")
End Sub

Sub Main()
' Test the function
Check()
End Sub
End Module