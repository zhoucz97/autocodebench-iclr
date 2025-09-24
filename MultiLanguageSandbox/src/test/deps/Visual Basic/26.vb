Module Module1
Function GenerateMagicSquare(ByVal n As Integer) As Integer(,)
    ' Check if n is odd
    If n Mod 2 = 0 Then
        Throw New ArgumentException("N must be an odd integer.")
    End If

    ' Initialize the magic square with zeros
    Dim magicSquare(n - 1, n - 1) As Integer

    ' Starting position
    Dim row As Integer = 0
    Dim col As Integer = n \ 2

    ' Fill the magic square
    For i As Integer = 1 To n * n
        magicSquare(row, col) = i

        ' Calculate the next position
        Dim nextRow As Integer = row - 1
        Dim nextCol As Integer = col + 1

        ' If the next position is out of bounds or already filled, move down
        If nextRow < 0 OrElse nextCol >= n OrElse magicSquare(nextRow, nextCol) <> 0 Then
            nextRow = row + 1
            nextCol = col
        End If

        ' Update the current position
        row = nextRow
        col = nextCol
    Next

    Return magicSquare
End Function

Sub CheckMagicSquare(ByVal testFunction As Func(Of Integer, Integer(,)))
Dim magicSquare3x3 As Integer(,) = testFunction(3)
Debug.Assert(magicSquare3x3.GetLength(0) = 3 And magicSquare3x3.GetLength(1) = 3)
Debug.Assert(magicSquare3x3.Cast(Of Integer)().Sum() = 45)

Dim magicSquare5x5 As Integer(,) = testFunction(5)
Debug.Assert(magicSquare5x5.GetLength(0) = 5 And magicSquare5x5.GetLength(1) = 5)
Debug.Assert(magicSquare5x5.Cast(Of Integer)().Sum() = 325)

Console.WriteLine("Test cases passed.")
End Sub

Sub Main()
CheckMagicSquare(AddressOf GenerateMagicSquare)
End Sub
End Module