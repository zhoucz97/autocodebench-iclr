Imports System.Collections.Generic
Module Module1
Function FindIntersection(ByVal nums1 As List(Of Integer), ByVal nums2 As List(Of Integer)) As List(Of Integer)
    ' Create dictionaries to count occurrences of each number in both lists
    Dim countDict1 As New Dictionary(Of Integer, Integer)
    Dim countDict2 As New Dictionary(Of Integer, Integer)
    
    ' Count occurrences in nums1
    For Each num As Integer In nums1
        If countDict1.ContainsKey(num) Then
            countDict1(num) += 1
        Else
            countDict1.Add(num, 1)
        End If
    Next
    
    ' Count occurrences in nums2
    For Each num As Integer In nums2
        If countDict2.ContainsKey(num) Then
            countDict2(num) += 1
        Else
            countDict2.Add(num, 1)
        End If
    Next
    
    ' Find the intersection
    Dim result As New List(Of Integer)
    
    ' Iterate through the keys in the first dictionary
    For Each key As Integer In countDict1.Keys
        If countDict2.ContainsKey(key) Then
            ' Add the number min(count1, count2) times
            Dim minCount As Integer = Math.Min(countDict1(key), countDict2(key))
            For i As Integer = 1 To minCount
                result.Add(key)
            Next
        End If
    Next
    
    Return result
End Function

    Sub Check(ByVal testFunction As Func(Of List(Of Integer), List(Of Integer), List(Of Integer)))
        ' Test cases
        ' Check if the intersection of [1, 2, 2, 1] and [2, 2] is [2]
        ' >>> FindIntersection([1, 2, 2, 1], [2, 2])
        ' [2]
        Debug.Assert(testFunction(New List(Of Integer) From {1, 2, 2, 1}, New List(Of Integer) From {2, 2}).SequenceEqual(New List(Of Integer) From {2}))

        ' Check if the intersection of [4, 9, 5] and [9, 4, 9, 8, 4] is [9, 4] or [4, 9]
        ' >>> FindIntersection([4, 9, 5], [9, 4, 9, 8, 4])
        ' [4, 9] or [9, 4]
        Debug.Assert(New HashSet(Of Integer)(testFunction(New List(Of Integer) From {4, 9, 5}, New List(Of Integer) From {9, 4, 9, 8, 4})).SetEquals(New HashSet(Of Integer) From {4, 9}))

        Console.WriteLine("FindIntersection function passed all tests")
    End Sub

    Sub Main()
        ' Testing the FindIntersection function
        Check(AddressOf FindIntersection)
    End Sub
End Module