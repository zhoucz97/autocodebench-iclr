Module Module1
Function TotalPoisonedDuration(ByVal timeSeries As List(Of Integer), ByVal duration As Integer) As Integer
    If timeSeries Is Nothing OrElse timeSeries.Count = 0 Then
        Return 0
    End If
    
    Dim totalDuration As Integer = 0
    
    For i As Integer = 0 To timeSeries.Count - 2
        Dim currentAttackTime As Integer = timeSeries(i)
        Dim nextAttackTime As Integer = timeSeries(i + 1)
        
        ' Calculate the duration between this attack and the next one
        Dim interval As Integer = nextAttackTime - currentAttackTime
        
        ' Add the minimum of the interval or the poison duration
        totalDuration += Math.Min(interval, duration)
    Next
    
    ' Add the full duration for the last attack
    totalDuration += duration
    
    Return totalDuration
End Function

Sub Check()
Debug.Assert(TotalPoisonedDuration(New List(Of Integer) From {1, 4}, 2) = 4)
Debug.Assert(TotalPoisonedDuration(New List(Of Integer) From {1, 2}, 2) = 3)
Debug.Assert(TotalPoisonedDuration(New List(Of Integer) From {1, 2, 3, 4, 5}, 5) = 9)
Debug.Assert(TotalPoisonedDuration(New List(Of Integer) From {}, 5) = 0)
Debug.Assert(TotalPoisonedDuration(New List(Of Integer) From {1, 2, 3, 10, 11}, 4) = 11)
Debug.Assert(TotalPoisonedDuration(New List(Of Integer) From {1, 1, 1, 1}, 3) = 3)

Console.WriteLine("Pass")
End Sub
Sub Main()
' Test the function
Check()
End Sub
End Module