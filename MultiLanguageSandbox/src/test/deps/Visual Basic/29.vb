Imports System
Module Module1
Function CalculateMatchScores(ByVal matchResults As String) As Tuple(Of List(Of String), List(Of String))
    Dim elevenPointScores As New List(Of String)
    Dim twentyOnePointScores As New List(Of String)
    
    Dim playerScore11 As Integer = 0
    Dim opponentScore11 As Integer = 0
    Dim playerScore21 As Integer = 0
    Dim opponentScore21 As Integer = 0
    
    For Each c As Char In matchResults
        If c = "E"c Then
            ' End of input, add current scores to lists
            elevenPointScores.Add($"{playerScore11}:{opponentScore11}")
            twentyOnePointScores.Add($"{playerScore21}:{opponentScore21}")
            Exit For
        ElseIf c = "W"c Then
            ' Player wins a point
            playerScore11 += 1
            playerScore21 += 1
            
            ' Check for game end in 11-point system
            If playerScore11 >= 11 AndAlso Math.Abs(playerScore11 - opponentScore11) >= 2 Then
                elevenPointScores.Add($"{playerScore11}:{opponentScore11}")
                playerScore11 = 0
                opponentScore11 = 0
            End If
            
            ' Check for game end in 21-point system
            If playerScore21 >= 21 AndAlso Math.Abs(playerScore21 - opponentScore21) >= 2 Then
                twentyOnePointScores.Add($"{playerScore21}:{opponentScore21}")
                playerScore21 = 0
                opponentScore21 = 0
            End If
        ElseIf c = "L"c Then
            ' Opponent wins a point
            opponentScore11 += 1
            opponentScore21 += 1
            
            ' Check for game end in 11-point system
            If opponentScore11 >= 11 AndAlso Math.Abs(playerScore11 - opponentScore11) >= 2 Then
                elevenPointScores.Add($"{playerScore11}:{opponentScore11}")
                playerScore11 = 0
                opponentScore11 = 0
            End If
            
            ' Check for game end in 21-point system
            If opponentScore21 >= 21 AndAlso Math.Abs(playerScore21 - opponentScore21) >= 2 Then
                twentyOnePointScores.Add($"{playerScore21}:{opponentScore21}")
                playerScore21 = 0
                opponentScore21 = 0
            End If
        End If
    Next
    
    Return Tuple.Create(elevenPointScores, twentyOnePointScores)
End Function

Sub Main()
' Testing function
Check(AddressOf CalculateMatchScores)
End Sub

Sub Check(ByVal testFunction As Func(Of String, Tuple(Of List(Of String), List(Of String))))
Dim result As Tuple(Of List(Of String), List(Of String))

result = testFunction("WWLWE")
Debug.Assert(result.Item1.SequenceEqual(New List(Of String) From {"3:1"}) AndAlso result.Item2.SequenceEqual(New List(Of String) From {"3:1"}))

result = testFunction("LWWWWLWE")
Debug.Assert(result.Item1.SequenceEqual(New List(Of String) From {"5:2"}) AndAlso result.Item2.SequenceEqual(New List(Of String) From {"5:2"}))

result = testFunction("WWWLWLWLWLWE")
Debug.Assert(result.Item1.SequenceEqual(New List(Of String) From {"7:4"}) AndAlso result.Item2.SequenceEqual(New List(Of String) From {"7:4"}))

Console.WriteLine("All test cases passed")
End Sub
End Module