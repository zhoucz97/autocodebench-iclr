Module Module1
Function FindIntervals(ByVal nums As List(Of Integer)) As List(Of String)
    Dim result As New List(Of String)
    
    If nums.Count = 0 Then
        Return result
    End If
    
    Dim start As Integer = nums(0)
    Dim prev As Integer = nums(0)
    
    For i As Integer = 1 To nums.Count - 1
        If nums(i) = prev + 1 Then
            ' Continue the current interval
            prev = nums(i)
        Else
            ' End the current interval and start a new one
            If start = prev Then
                result.Add(start.ToString())
            Else
                result.Add(start & "->" & prev)
            End If
            start = nums(i)
            prev = nums(i)
        End If
    Next
    
    ' Add the last interval
    If start = prev Then
        result.Add(start.ToString())
    Else
        result.Add(start & "->" & prev)
    End If
    
    Return result
End Function

    Sub Check(ByVal testFunction As Func(Of List(Of Integer), List(Of String)))
        ' Test cases
        Debug.Assert(testFunction(New List(Of Integer) From {0, 1, 2, 4, 5, 7}).SequenceEqual(New List(Of String) From {"0->2", "4->5", "7"}))
        Debug.Assert(testFunction(New List(Of Integer) From {0, 2, 3, 4, 6, 8, 9}).SequenceEqual(New List(Of String) From {"0", "2->4", "6", "8->9"}))
        Debug.Assert(testFunction(New List(Of Integer) From {}).SequenceEqual(New List(Of String) From {}))
        Debug.Assert(testFunction(New List(Of Integer) From {1}).SequenceEqual(New List(Of String) From {"1"}))

        Console.WriteLine("FindIntervals function passed all tests")
    End Sub

    Sub Main()
        ' Testing the FindIntervals function
        Check(AddressOf FindIntervals)
    End Sub
End Module