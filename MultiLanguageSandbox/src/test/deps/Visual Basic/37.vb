Module Module1

Function IsNaN(value As Double) As Boolean
    ' Returns True if the value is NaN, False otherwise
    Return value <> value
End Function


    ' Subroutine to check the correctness of the MoveZerosToEnd function
    Sub Check(ByVal testProcedure As Action(Of List(Of Integer)))
        Dim testList As New List(Of Integer) From {0, 1, 0, 3, 12}
        testProcedure(testList)
        Debug.Assert(testList.SequenceEqual(New List(Of Integer) From {1, 3, 12, 0, 0}))

        testList = New List(Of Integer) From {0, 0, 1}
        testProcedure(testList)
        Debug.Assert(testList.SequenceEqual(New List(Of Integer) From {1, 0, 0}))

        Console.WriteLine("MoveZerosToEnd function passed all tests")
    End Sub

    Sub Main()
        ' Testing the MoveZerosToEnd function
        Check(AddressOf MoveZerosToEnd)
    End Sub
End Module