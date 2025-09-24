Imports System
Module Module1
Function MaximizeMinimumDistance(ByVal positions As List(Of Integer), ByVal cows As Integer) As Integer
        ' Sort the positions to make it easier to calculate distances
        positions.Sort()
        
        Dim left As Integer = 1
        Dim right As Integer = positions(positions.Count - 1) - positions(0)
        Dim result As Integer = 0
        
        While left <= right
            Dim mid As Integer = left + (right - left) \ 2
            If CanPlaceCows(positions, cows, mid) Then
                result = mid
                left = mid + 1
            Else
                right = mid - 1
            End If
        End While
        
        Return result
    End Function

Sub Check(ByVal testFunction As Func(Of List(Of Integer), Integer, Integer))
Debug.Assert(testFunction(New List(Of Integer) From {1, 3, 5, 7, 9, 11}, 2) = 10)
Debug.Assert(testFunction(New List(Of Integer) From {1, 2, 4, 8, 9}, 3) = 3)
Debug.Assert(testFunction(New List(Of Integer) From {1, 2, 3, 4, 5}, 3) = 2)
Debug.Assert(testFunction(New List(Of Integer) From {1, 5, 9, 14, 20}, 4) = 5)
Debug.Assert(testFunction(New List(Of Integer) From {10, 20, 30, 40, 50}, 2) = 40)
Debug.Assert(testFunction(New List(Of Integer) From {5, 6, 7, 8, 9, 10}, 3) = 2)
Debug.Assert(testFunction(New List(Of Integer) From {3, 4, 5, 6, 10, 12}, 4) = 2)

Console.WriteLine("All test cases passed")
End Sub

Sub Main()
' Run test function
Check(AddressOf MaximizeMinimumDistance)
End Sub
End Module