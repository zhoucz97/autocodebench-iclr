Module Module1
Function CalculateFinalScore(ByVal scores As List(Of Integer)) As Double
    If scores Is Nothing OrElse scores.Count < 3 Then
        Throw New ArgumentException("The list must contain at least 3 scores.")
    End If
    
    ' Create a copy of the list to avoid modifying the original
    Dim sortedScores = scores.OrderBy(Function(x) x).ToList()
    
    ' Remove the first (lowest) and last (highest) elements
    sortedScores.RemoveAt(0)
    sortedScores.RemoveAt(sortedScores.Count - 1)
    
    ' Calculate the average of the remaining scores
    Dim sum As Double = 0
    For Each score In sortedScores
        sum += score
    Next
    
    Dim average As Double = sum / sortedScores.Count
    
    ' Round to two decimal places
    Return Math.Round(average, 2)
End Function

Sub Check(ByVal testFunction As Func(Of List(Of Integer), Double))
Debug.Assert(testFunction(New List(Of Integer) From {8, 9, 7, 6, 5}) = 7.00)
Debug.Assert(testFunction(New List(Of Integer) From {10, 10, 9, 8, 7, 7}) = 8.50)
Debug.Assert(testFunction(New List(Of Integer) From {5, 5, 5, 5, 5}) = 5)
Debug.Assert(testFunction(New List(Of Integer) From {0, 10, 5, 5, 5}) = 5)
Debug.Assert(testFunction(New List(Of Integer) From {10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0}) = 5)
Debug.Assert(testFunction(New List(Of Integer) From {6, 6, 6, 6, 6, 6, 7}) = 6)

Console.WriteLine("All test cases passed.")
End Sub
Sub Main()
' Test the function
Check(AddressOf CalculateFinalScore)
End Sub
End Module