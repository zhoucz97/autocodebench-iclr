Imports System.Collections.Generic
Module Module1
Function IntersectAndSortArrays(ByVal nums1 As List(Of Integer), ByVal nums2 As List(Of Integer)) As List(Of Integer)
    ' Create dictionaries to count occurrences of each number in both lists
    Dim count1 As New Dictionary(Of Integer, Integer)
    Dim count2 As New Dictionary(Of Integer, Integer)
    
    ' Count occurrences in nums1
    For Each num As Integer In nums1
        If count1.ContainsKey(num) Then
            count1(num) += 1
        Else
            count1.Add(num, 1)
        End If
    Next
    
    ' Count occurrences in nums2
    For Each num As Integer In nums2
        If count2.ContainsKey(num) Then
            count2(num) += 1
        Else
            count2.Add(num, 1)
        End If
    Next
    
    ' Find the intersection and determine the minimum counts
    Dim result As New List(Of Integer)
    
    ' Iterate through the keys of the first dictionary
    For Each key As Integer In count1.Keys
        If count2.ContainsKey(key) Then
            ' Take the minimum count between the two
            Dim minCount As Integer = Math.Min(count1(key), count2(key))
            ' Add the number minCount times to the result
            For i As Integer = 1 To minCount
                result.Add(key)
            Next
        End If
    Next
    
    ' Sort the result in ascending order
    result.Sort()
    
    Return result
End Function

' Subroutine for testing the function's correctness
Sub Check()
Debug.Assert(IntersectAndSortArrays(New List(Of Integer) From {1, 2, 2, 1}, New List(Of Integer) From {2, 2}).SequenceEqual(New List(Of Integer) From {2, 2}))
Debug.Assert(IntersectAndSortArrays(New List(Of Integer) From {4, 9, 5}, New List(Of Integer) From {9, 4, 9, 8, 4}).SequenceEqual(New List(Of Integer) From {4, 9}))
Debug.Assert(IntersectAndSortArrays(New List(Of Integer) From {1, 2, 2, 1}, New List(Of Integer) From {1, 1, 2, 3, 4}).SequenceEqual(New List(Of Integer) From {1, 1, 2}))

Console.WriteLine("Pass")
End Sub

Sub Main()
' Test the function
Check()
End Sub
End Module