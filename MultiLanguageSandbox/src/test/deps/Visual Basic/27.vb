Imports System
Module Module1
Function IsLuckyWord(ByVal word As String) As Tuple(Of String, Integer)
        ' Create a dictionary to count letter frequencies
        Dim frequencyDict As New Dictionary(Of Char, Integer)
        
        ' Count each letter's frequency (case insensitive)
        For Each c As Char In word.ToLower()
            If Char.IsLetter(c) Then
                If frequencyDict.ContainsKey(c) Then
                    frequencyDict(c) += 1
                Else
                    frequencyDict.Add(c, 1)
                End If
            End If
        Next
        
        ' If no letters found, return No Answer
        If frequencyDict.Count = 0 Then
            Return Tuple.Create("No Answer", 0)
        End If
        
        ' Find max and min frequencies
        Dim maxFreq As Integer = frequencyDict.Values.Max()
        Dim minFreq As Integer = frequencyDict.Values.Min()
        Dim difference As Integer = maxFreq - minFreq
        
        ' Check if difference is prime
        If IsPrime(difference) Then
            Return Tuple.Create("Lucky Word", difference)
        Else
            Return Tuple.Create("No Answer", 0)
        End If
    End Function

Sub Main()
TestIsLuckyWord()
End Sub

Sub TestIsLuckyWord()
Console.WriteLine("Test Case 1: 'apple'")
Debug.Assert(IsLuckyWord("apple").Equals(New Tuple(Of String, Integer)("No Answer", 0)))

Console.WriteLine("Test Case 2: 'banana'")
Debug.Assert(IsLuckyWord("banana").Equals(New Tuple(Of String, Integer)("Lucky Word", 2)))

Console.WriteLine("Test Case 3: 'character'")
Debug.Assert(IsLuckyWord("character").Equals(New Tuple(Of String, Integer)("No Answer", 0)))

Console.WriteLine("Test Case 4: 'foundation'")
Debug.Assert(IsLuckyWord("foundation").Equals(New Tuple(Of String, Integer)("No Answer", 0)))

Console.WriteLine("Test Case 5: 'zebra'")
Debug.Assert(IsLuckyWord("zebra").Equals(New Tuple(Of String, Integer)("No Answer", 0)))

Console.WriteLine("All tests passed.")
End Sub
End Module