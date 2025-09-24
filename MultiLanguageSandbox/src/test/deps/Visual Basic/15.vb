Module Module1
Function TrianglePerimeter(ByVal x1 As Double, ByVal y1 As Double, ByVal x2 As Double, ByVal y2 As Double, ByVal x3 As Double, ByVal y3 As Double) As Double
    ' Calculate the lengths of the three sides
    Dim side1 As Double = Math.Sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
    Dim side2 As Double = Math.Sqrt((x3 - x2) ^ 2 + (y3 - y2) ^ 2)
    Dim side3 As Double = Math.Sqrt((x1 - x3) ^ 2 + (y1 - y3) ^ 2)
    
    ' Return the sum of the three sides (perimeter)
    Return side1 + side2 + side3
End Function

Sub Main()
' Test the TrianglePerimeter function
Debug.Assert(TrianglePerimeter(0, 0, 0, 4, 3, 0) = 12.0)
Debug.Assert(TrianglePerimeter(-1, -1, 1, -1, 0, 1) = 6.47)
Debug.Assert(TrianglePerimeter(1, 2, 3, 4, 5, 6) = 11.31)
Debug.Assert(TrianglePerimeter(2, 2, 2, 5, 5, 2) = 10.24)
Debug.Assert(TrianglePerimeter(0, 0, 1, 0, 0, 1) = 3.41)
Debug.Assert(TrianglePerimeter(-2, -3, 4, -3, 1, 2) = 17.66)
Debug.Assert(TrianglePerimeter(0.5, 0.5, 2.5, 0.5, 1.5, 2) = 5.61)

Console.WriteLine("All test cases passed")
End Sub

End Module