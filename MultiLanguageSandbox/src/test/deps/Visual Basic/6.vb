Module Module1
Function IslandPerimeter(ByVal grid As List(Of List(Of Integer))) As Integer
    Dim perimeter As Integer = 0
    Dim rows As Integer = grid.Count
    If rows = 0 Then Return 0
    Dim cols As Integer = grid(0).Count
    
    For i As Integer = 0 To rows - 1
        For j As Integer = 0 To cols - 1
            If grid(i)(j) = 1 Then
                ' Check top
                If i = 0 OrElse grid(i - 1)(j) = 0 Then
                    perimeter += 1
                End If
                ' Check bottom
                If i = rows - 1 OrElse grid(i + 1)(j) = 0 Then
                    perimeter += 1
                End If
                ' Check left
                If j = 0 OrElse grid(i)(j - 1) = 0 Then
                    perimeter += 1
                End If
                ' Check right
                If j = cols - 1 OrElse grid(i)(j + 1) = 0 Then
                    perimeter += 1
                End If
            End If
        Next
    Next
    
    Return perimeter
End Function

Sub Check()
Debug.Assert(IslandPerimeter(New List(Of List(Of Integer)) From {New List(Of Integer) From {0, 1, 0, 0}, New List(Of Integer) From {1, 1, 1, 0}, New List(Of Integer) From {0, 1, 0, 0}, New List(Of Integer) From {1, 1, 0, 0}}) = 16)
Debug.Assert(IslandPerimeter(New List(Of List(Of Integer)) From {New List(Of Integer) From {1}}) = 4)
Debug.Assert(IslandPerimeter(New List(Of List(Of Integer)) From {New List(Of Integer) From {1, 0}}) = 4)

Console.WriteLine("Pass")
End Sub

Sub Main()
' Test the function
Check()
End Sub
End Module