Module Module1
Function CalculateAttendanceDifferences(ByVal area As Integer, ByVal peoplePerSquareMeter As Integer, ByVal reportedNumbers As List(Of Integer)) As List(Of Integer)
    ' Calculate the actual attendance based on area and people per square meter
    Dim actualAttendance As Integer = area * peoplePerSquareMeter
    
    ' Create a list to store the differences
    Dim differences As New List(Of Integer)
    
    ' Calculate the difference for each reported number
    For Each reportedNumber As Integer In reportedNumbers
        differences.Add(reportedNumber - actualAttendance)
    Next
    
    Return differences
End Function

Sub Check(ByVal testFunction As Func(Of Integer, Integer, List(Of Integer), List(Of Integer)))
Debug.Assert(testFunction(15, 10, New List(Of Integer) From {150, 155, 145, 160, 150}).SequenceEqual(New List(Of Integer) From {0, 5, -5, 10, 0}))
Debug.Assert(testFunction(8, 12, New List(Of Integer) From {96, 100, 90, 104, 92}).SequenceEqual(New List(Of Integer) From {0, 4, -6, 8, -4}))
Debug.Assert(testFunction(25, 4, New List(Of Integer) From {100, 95, 105, 110, 90}).SequenceEqual(New List(Of Integer) From {0, -5, 5, 10, -10}))

Console.WriteLine("All test cases passed")
End Sub
Sub Main()
' Testing the function
Check(AddressOf CalculateAttendanceDifferences)
End Sub

End Module