Module Module1
Function MaxKangarooMoves(ByVal positions As List(Of Integer)) As Integer
    ' Sort the positions to easily identify left, middle, and right kangaroos
    Dim sortedPositions = positions.OrderBy(Function(x) x).ToList()
    Dim left = sortedPositions(0)
    Dim middle = sortedPositions(1)
    Dim right = sortedPositions(2)
    
    Dim moves = 0
    
    While True
        ' Check if left can move right
        If left + 1 < middle Then
            left += 1
            moves += 1
        ' Check if right can move left
        ElseIf right - 1 > middle Then
            right -= 1
            moves += 1
        Else
            Exit While
        End If
    End While
    
    Return moves
End Function

Sub Check(ByVal testFunction As Func(Of List(Of Integer), Integer))
Debug.Assert(testFunction(New List(Of Integer) From {1, 4, 7}) = 2)
Debug.Assert(testFunction(New List(Of Integer) From {3, 5, 8}) = 2)
Debug.Assert(testFunction(New List(Of Integer) From {2, 3, 6}) = 2)
Debug.Assert(testFunction(New List(Of Integer) From {1, 2, 5}) = 2)
Debug.Assert(testFunction(New List(Of Integer) From {10, 13, 19}) = 5)
Debug.Assert(testFunction(New List(Of Integer) From {4, 7, 10}) = 2)
Debug.Assert(testFunction(New List(Of Integer) From {5, 9, 12}) = 3)

Console.WriteLine("pass")
End Sub
Sub Main()
' Testing the function
Check(AddressOf MaxKangarooMoves)
End Sub
End Module