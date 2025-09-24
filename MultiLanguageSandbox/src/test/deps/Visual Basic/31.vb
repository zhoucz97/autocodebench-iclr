Module Module1
Function SortVotes(ByVal n As Integer, ByVal m As Integer, ByVal votes As List(Of Integer)) As List(Of Integer)
    ' Sort the votes list in ascending order
    votes.Sort()
    
    ' Return the sorted list
    Return votes
End Function

Sub Check(ByVal testFunction As Func(Of Integer, Integer, List(Of Integer), List(Of Integer)))
Debug.Assert(testFunction(3, 5, New List(Of Integer) From {3, 1, 2, 3, 2}).SequenceEqual(New List(Of Integer) From {1, 2, 2, 3, 3}))
Debug.Assert(testFunction(4, 6, New List(Of Integer) From {4, 4, 2, 1, 3, 2}).SequenceEqual(New List(Of Integer) From {1, 2, 2, 3, 4, 4}))
Debug.Assert(testFunction(2, 4, New List(Of Integer) From {2, 1, 2, 1}).SequenceEqual(New List(Of Integer) From {1, 1, 2, 2}))
Debug.Assert(testFunction(5, 3, New List(Of Integer) From {5, 3, 4}).SequenceEqual(New List(Of Integer) From {3, 4, 5}))

Console.WriteLine("All test cases passed")
End Sub

Sub Main()
' Test the SortVotes function
Check(AddressOf SortVotes)
End Sub

End Module