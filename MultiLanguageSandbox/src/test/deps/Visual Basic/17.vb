Module Module1
Function CalculatePeaches(ByVal days As Integer) As Integer
    Dim peaches As Integer = 1 ' Start with 1 peach on the last day
    
    For i As Integer = 1 To days - 1
        peaches = (peaches + 1) * 2
    Next
    
    Return peaches
End Function

Sub Main()
    ' Test the CalculatePeaches function with various scenarios
    Debug.Assert(CalculatePeaches(3) = 10)
    Debug.Assert(CalculatePeaches(5) = 46)
    Debug.Assert(CalculatePeaches(6) = 94)
    Debug.Assert(CalculatePeaches(2) = 4)
    Debug.Assert(CalculatePeaches(7) = 190)

    Console.WriteLine("All test cases passed")
End Sub
End Module