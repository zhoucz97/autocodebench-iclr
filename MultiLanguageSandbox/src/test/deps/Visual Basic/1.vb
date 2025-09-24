Module Module1
Function HasCloseElements(ByVal numbers As List(Of Double), ByVal threshold As Double) As Boolean
    ' Sort the list to easily find the closest elements
    numbers.Sort()
    
    ' Iterate through the sorted list and check adjacent elements
    For i As Integer = 0 To numbers.Count - 2
        If Math.Abs(numbers(i + 1) - numbers(i)) < threshold Then
            Return True
        End If
    Next
    
    Return False
End Function

Sub Check(ByVal testFunction As Func(Of List(Of Double), Double, Boolean))
Debug.Assert(testFunction(New List(Of Double) From {1.0, 2.0, 3.9, 4.0, 5.0, 2.2}, 0.3) = True)
Debug.Assert(testFunction(New List(Of Double) From {1.0, 2.0, 3.9, 4.0, 5.0, 2.2}, 0.05) = False)
Debug.Assert(testFunction(New List(Of Double) From {1.0, 2.0, 5.9, 4.0, 5.0}, 0.95) = True)
Debug.Assert(testFunction(New List(Of Double) From {1.0, 2.0, 5.9, 4.0, 5.0}, 0.8) = False)
Debug.Assert(testFunction(New List(Of Double) From {1.0, 2.0, 3.0, 4.0, 5.0, 2.0}, 0.1) = True)
Debug.Assert(testFunction(New List(Of Double) From {1.1, 2.2, 3.1, 4.1, 5.1}, 1.0) = True)
Debug.Assert(testFunction(New List(Of Double) From {1.1, 2.2, 3.1, 4.1, 5.1}, 0.5) = False)

Console.WriteLine("pass")
End Sub
Sub Main()
' 测试函数
Check(AddressOf HasCloseElements)
End Sub
End Module