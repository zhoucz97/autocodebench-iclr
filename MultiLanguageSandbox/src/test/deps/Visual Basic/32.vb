Module Module1
Function MinimumSwapsToSort(ByVal carriages As List(Of Integer)) As Integer
    Dim swaps As Integer = 0
    Dim n As Integer = carriages.Count
    
    For i As Integer = 0 To n - 1
        For j As Integer = i + 1 To n - 1
            If carriages(i) > carriages(j) Then
                swaps += 1
            End If
        Next
    Next
    
    Return swaps
End Function

Sub Check(ByVal testFunction As Func(Of List(Of Integer), Integer))
Debug.Assert(testFunction(New List(Of Integer) From {3, 1, 2}) = 2)
Debug.Assert(testFunction(New List(Of Integer) From {4, 3, 2, 1}) = 6)
Debug.Assert(testFunction(New List(Of Integer) From {1, 5, 4, 3, 2}) = 6)
Debug.Assert(testFunction(New List(Of Integer) From {1, 2, 3, 4, 5}) = 0)
Debug.Assert(testFunction(New List(Of Integer) From {5, 4, 3, 2, 1}) = 10)
Debug.Assert(testFunction(New List(Of Integer) From {2, 3, 4, 5, 1}) = 4)
Debug.Assert(testFunction(New List(Of Integer) From {6, 5, 4, 3, 2, 1}) = 15)

Console.WriteLine("Pass")
End Sub
Sub Main()
' Test the function
Check(AddressOf MinimumSwapsToSort)
End Sub
End Module