Module Module1
Function ThirdLargest(ByVal numbers As List(Of Integer)) As Integer
    ' Create a sorted copy of the list in descending order
    Dim sortedNumbers As List(Of Integer) = numbers.OrderByDescending(Function(x) x).ToList()
    
    ' Remove duplicates by converting to a HashSet and back to List
    Dim uniqueNumbers As List(Of Integer) = sortedNumbers.Distinct().ToList()
    
    ' Determine which index to return (minimum of 0 or count - 1)
    Dim index As Integer = Math.Min(2, uniqueNumbers.Count - 1)
    
    Return uniqueNumbers(index)
End Function

Sub Check()
Debug.Assert(ThirdLargest(New List(Of Integer) From {2, 3, 1}) = 1)
Debug.Assert(ThirdLargest(New List(Of Integer) From {1}) = 1)
Debug.Assert(ThirdLargest(New List(Of Integer) From {1, 2}) = 2)
Debug.Assert(ThirdLargest(New List(Of Integer) From {2, 2, 3, 1}) = 1)
Debug.Assert(ThirdLargest(New List(Of Integer) From {5, 2, 4}) = 2)
Debug.Assert(ThirdLargest(New List(Of Integer) From {5, 5, 5, 1}) = 5)
Debug.Assert(ThirdLargest(New List(Of Integer) From {10, 9, 8, 7, 6}) = 8)

Console.WriteLine("Pass")
End Sub

Sub Main()
' Test the function
Check()
End Sub
End Module