Imports System.Collections.Generic
Module Module1
Function PrimeNumbersSumWithinLimit(ByVal limit As Integer) As List(Of Integer)
        Dim primes As New List(Of Integer)
        Dim sum As Integer = 0
        Dim count As Integer = 0
        Dim currentNumber As Integer = 2
        
        While True
            If IsPrime(currentNumber) Then
                If sum + currentNumber > limit Then
                    Exit While
                End If
                sum += currentNumber
                primes.Add(currentNumber)
                count += 1
            End If
            currentNumber += 1
        End While
        
        primes.Add(count)
        Return primes
    End Function

Sub Check(ByVal testFunction As Func(Of Integer, List(Of Integer)))
Dim result As List(Of Integer)

result = testFunction(10)
Debug.Assert(result.SequenceEqual(New List(Of Integer) From {2, 3, 5,3}))

result = testFunction(20)
Debug.Assert(result.SequenceEqual(New List(Of Integer) From {2, 3, 5, 7, 4}))

result = testFunction(30)
Debug.Assert(result.SequenceEqual(New List(Of Integer) From {2, 3, 5, 7, 11, 5}))

result = testFunction(50)
Debug.Assert(result.SequenceEqual(New List(Of Integer) From {2, 3, 5, 7, 11, 13, 6}))

Console.WriteLine("Tests passed")
End Sub

Sub Main()
' Testing function
Check(AddressOf PrimeNumbersSumWithinLimit)
End Sub

End Module