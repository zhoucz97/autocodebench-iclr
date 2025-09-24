Imports System
Imports System.Collections.Generic
Imports System.Linq
Imports System.Diagnostics

Namespace QuickSortProgram
    Class Program
        Private Shared Function Quicksort(list As List(Of Integer)) As List(Of Integer)
            If list Is Nothing OrElse list.Count <= 1 Then
                Return If(list, New List(Of Integer)())
            End If

            Dim pivot As Integer = list(0)
            Dim rest As List(Of Integer) = list.Skip(1).ToList()
            Dim less As List(Of Integer) = rest.Where(Function(x) x < pivot).ToList()
            Dim greater As List(Of Integer) = rest.Where(Function(x) x >= pivot).ToList()

            Return Quicksort(less).Concat(New List(Of Integer) From {pivot}).Concat(Quicksort(greater)).ToList()
        End Function

        Private Shared Sub CheckQuicksort()
            Debug.Assert(Quicksort(New List(Of Integer) From {3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5}).SequenceEqual(New List(Of Integer) From {1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 9}))
            Debug.Assert(Quicksort(New List(Of Integer) From {5, 4, 3, 2, 1}).SequenceEqual(New List(Of Integer) From {1, 2, 3, 4, 5}))
            Debug.Assert(Quicksort(New List(Of Integer)()).SequenceEqual(New List(Of Integer)()))
            Debug.Assert(Quicksort(New List(Of Integer) From {1}).SequenceEqual(New List(Of Integer) From {1}))
        End Sub

        Shared Sub Main(args As String())
            CheckQuicksort()
        End Sub
    End Class
End Namespace
