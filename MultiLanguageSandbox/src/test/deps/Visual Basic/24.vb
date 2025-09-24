Imports System
Module Module1
Function FindPalindromePrimes(ByVal a As Integer, ByVal b As Integer) As List(Of Integer)
        Dim result As New List(Of Integer)()
        
        For num As Integer = a To b
            If IsPrime(num) AndAlso IsPalindrome(num) Then
                result.Add(num)
            End If
        Next
        
        Return result
    End Function

Sub Check(ByVal testFunction As Func(Of Integer, Integer, List(Of Integer)))
Debug.Assert(testFunction(10, 100).SequenceEqual(New List(Of Integer) From {11}))
Debug.Assert(testFunction(300, 400).SequenceEqual(New List(Of Integer) From {313,353,373,383}))
Debug.Assert(testFunction(500, 600).SequenceEqual(New List(Of Integer) From {}))
Debug.Assert(testFunction(800, 900).SequenceEqual(New List(Of Integer) From {}))

Console.WriteLine("Tests passed")
End Sub

Sub Main()
' Test the function
Check(AddressOf FindPalindromePrimes)
End Sub
End Module