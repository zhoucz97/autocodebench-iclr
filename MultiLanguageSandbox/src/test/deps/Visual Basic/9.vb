Imports System.Collections.Generic
Module Module1
Function NextGreaterElement(ByVal nums1 As List(Of Integer), ByVal nums2 As List(Of Integer)) As List(Of Integer)
    Dim result As New List(Of Integer)
    Dim stack As New Stack(Of Integer)
    Dim nextGreater As New Dictionary(Of Integer, Integer)
    
    ' First pass through nums2 to find next greater elements using a stack
    For Each num As Integer In nums2
        While stack.Count > 0 AndAlso stack.Peek() < num
            nextGreater(stack.Pop()) = num
        End While
        stack.Push(num)
    Next
    
    ' Remaining elements in stack have no next greater element
    While stack.Count > 0
        nextGreater(stack.Pop()) = -1
    End While
    
    ' Build the result for nums1
    For Each num As Integer In nums1
        result.Add(nextGreater(num))
    Next
    
    Return result
End Function

Sub Check()
Debug.Assert(NextGreaterElement(New List(Of Integer) From {4, 1, 2}, New List(Of Integer) From {1, 3, 4, 2}).SequenceEqual(New List(Of Integer) From {-1, 3, -1}))
Debug.Assert(NextGreaterElement(New List(Of Integer) From {2, 4}, New List(Of Integer) From {1, 2, 3, 4}).SequenceEqual(New List(Of Integer) From {3, -1}))
Debug.Assert(NextGreaterElement(New List(Of Integer) From {1, 3}, New List(Of Integer) From {3, 1}).SequenceEqual(New List(Of Integer) From {-1, -1}))
Debug.Assert(NextGreaterElement(New List(Of Integer) From {}, New List(Of Integer) From {1, 2, 3, 4}).Count = 0)
Debug.Assert(NextGreaterElement(New List(Of Integer) From {1, 2}, New List(Of Integer) From {1, 2, 3}).SequenceEqual(New List(Of Integer) From {2, 3}))

Console.WriteLine("Pass")
End Sub

Sub Main()
' Test the function
Check()
End Sub
End Module