Module Module1
Function FindWordsSameRow(ByVal words As List(Of String)) As List(Of String)
    ' Define the three keyboard rows
    Dim row1 As String = "qwertyuiop"
    Dim row2 As String = "asdfghjkl"
    Dim row3 As String = "zxcvbnm"
    
    Dim result As New List(Of String)
    
    For Each word In words
        If word.Length = 0 Then Continue For ' Skip empty words
        
        ' Convert the word to lowercase for case-insensitive comparison
        Dim lowerWord As String = word.ToLower()
        
        ' Check which row the first character belongs to
        Dim firstChar As Char = lowerWord(0)
        Dim currentRow As String = ""
        
        If row1.Contains(firstChar) Then
            currentRow = row1
        ElseIf row2.Contains(firstChar) Then
            currentRow = row2
        ElseIf row3.Contains(firstChar) Then
            currentRow = row3
        Else
            Continue For ' Character not found in any row (shouldn't happen with valid input)
        End If
        
        ' Check if all characters are in the same row
        Dim allSameRow As Boolean = True
        For i As Integer = 1 To lowerWord.Length - 1
            If Not currentRow.Contains(lowerWord(i)) Then
                allSameRow = False
                Exit For
            End If
        Next
        
        If allSameRow Then
            result.Add(word)
        End If
    Next
    
    Return result
End Function

Sub Check()
Debug.Assert(FindWordsSameRow(New List(Of String) From {"Hello", "Alaska", "Dad", "Peace"}).SequenceEqual(New List(Of String) From {"Alaska", "Dad"}))
Debug.Assert(FindWordsSameRow(New List(Of String) From {"omk"}).Count = 0)
Debug.Assert(FindWordsSameRow(New List(Of String) From {"adsdf", "sfd"}).SequenceEqual(New List(Of String) From {"adsdf", "sfd"}))

Console.WriteLine("Pass")
End Sub

Sub Main()
' Test the function
Check()
End Sub
End Module