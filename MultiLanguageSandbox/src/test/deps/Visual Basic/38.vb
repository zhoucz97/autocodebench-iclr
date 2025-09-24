Module Module1
Function SumBetweenIndices(ByVal nums As List(Of Integer), ByVal left As Integer, ByVal right As Integer) As Integer
    Dim sum As Integer = 0
    
    ' Ensure left is not greater than right to avoid negative iteration
    If left > right Then
        Return 0 ' or you could throw an exception if preferred
    End If
    
    For i As Integer = left To right
        sum += nums(i)
    Next
    
    Return sum
End Function

    Sub Check(ByVal testFunction As Func(Of List(Of Integer), Integer, Integer, Integer))
        ' Test cases
        ' Check if the sum of elements between indices 1 and 3 in [1, 2, 3, 4] is 9
        ' >>> SumBetweenIndices([1, 2, 3, 4], 1, 3)
        ' 9
        Debug.Assert(testFunction(New List(Of Integer) From {1, 2, 3, 4}, 1, 3) = 9)

        ' Check if the sum of elements between indices 0 and 2 in [5, 10, 15, 20] is 30
        ' >>> SumBetweenIndices([5, 10, 15, 20], 0, 2)
        ' 30
        Debug.Assert(testFunction(New List(Of Integer) From {5, 10, 15, 20}, 0, 2) = 30)

        ' Check if the sum of all elements in [1, 1, 1, 1, 1] is 5
        ' >>> SumBetweenIndices([1, 1, 1, 1, 1], 0, 4)
        ' 5
        Debug.Assert(testFunction(New List(Of Integer) From {1, 1, 1, 1, 1}, 0, 4) = 5)

        Console.WriteLine("SumBetweenIndices function passed all tests")
    End Sub

    Sub Main()
        ' Testing the SumBetweenIndices function
        Check(AddressOf SumBetweenIndices)
    End Sub
End Module