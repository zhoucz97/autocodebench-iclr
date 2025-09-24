Module Module1
Function FindAutomorphicNumbers(ByVal start As Integer, ByVal endVal As Integer) As List(Of Integer)
    Dim automorphicNumbers As New List(Of Integer)
    
    For i As Integer = start To endVal
        Dim square As Long = CLng(i) * CLng(i) ' Use Long to avoid overflow for larger numbers
        Dim strNumber As String = i.ToString()
        Dim strSquare As String = square.ToString()
        
        ' Check if the square ends with the original number
        If strSquare.EndsWith(strNumber) Then
            automorphicNumbers.Add(i)
        End If
    Next
    
    Return automorphicNumbers
End Function

    Sub CheckAutomorphic(ByVal testFunction As Func(Of Integer, Integer, List(Of Integer)))
        ' Test cases
        Debug.Assert(testFunction(0, 10).SequenceEqual(New List(Of Integer) From {0, 1, 5, 6}))
        Debug.Assert(testFunction(25, 30).SequenceEqual(New List(Of Integer) From {25}))
        Debug.Assert(testFunction(75, 100).SequenceEqual(New List(Of Integer) From {76}))

        Console.WriteLine("FindAutomorphicNumbers function passed all tests")
    End Sub

    Sub Main()
        ' Testing the FindAutomorphicNumbers function
        CheckAutomorphic(AddressOf FindAutomorphicNumbers)
    End Sub
End Module