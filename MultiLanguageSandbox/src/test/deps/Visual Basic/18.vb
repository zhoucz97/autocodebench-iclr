Module Module1
Function MaxRectangleArea(ByVal lengths As List(Of Integer)) As Integer
    ' Sort the list to easily find pairs
    lengths.Sort()
    
    ' Check if the first two elements are the same and the last two are the same
    If lengths(0) = lengths(1) AndAlso lengths(2) = lengths(3) Then
        Return lengths(0) * lengths(2)
    ' Check if the first and third elements are the same and the second and fourth are the same
    ElseIf lengths(0) = lengths(2) AndAlso lengths(1) = lengths(3) Then
        Return lengths(0) * lengths(1)
    ' Check if the first and fourth elements are the same and the second and third are the same
    ElseIf lengths(0) = lengths(3) AndAlso lengths(1) = lengths(2) Then
        Return lengths(0) * lengths(1)
    Else
        ' This case shouldn't happen as per the problem statement, but handle it anyway
        Return 0
    End If
End Function

Sub Check(ByVal testFunction As Func(Of List(Of Integer), Integer))
' Test with a square
Debug.Assert(testFunction(New List(Of Integer) From {1, 1, 1, 1}) = 1)
' Test with a rectangle
Debug.Assert(testFunction(New List(Of Integer) From {2, 5, 3, 5}) = 10)
' Test with different lengths
Debug.Assert(testFunction(New List(Of Integer) From {4, 7, 4, 5}) = 20)
' Test with one side very small
Debug.Assert(testFunction(New List(Of Integer) From {1, 2, 10, 10}) = 10)
' Test with all sides different
Debug.Assert(testFunction(New List(Of Integer) From {2, 3, 6, 7}) = 12)

Console.WriteLine("pass")
End Sub
Sub Main()
' Testing the function
Check(AddressOf MaxRectangleArea)
End Sub
End Module