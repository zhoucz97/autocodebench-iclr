Module Module1
Function FindMissingNumber(ByVal nums As List(Of Integer)) As Integer
    ' Calculate the expected sum if no numbers were missing
    Dim n As Integer = nums.Count
    Dim expectedSum As Integer = n * (n + 1) \ 2
    
    ' Calculate the actual sum of the given numbers
    Dim actualSum As Integer = 0
    For Each num As Integer In nums
        actualSum += num
    Next
    
    ' The missing number is the difference between expected and actual sum
    Return expectedSum - actualSum
End Function

    Sub Check(ByVal testFunction As Func(Of List(Of Integer), Integer))
        ' Test cases
        Debug.Assert(testFunction(New List(Of Integer) From {3, 0, 1}) = 2)
        Debug.Assert(testFunction(New List(Of Integer) From {0, 1}) = 2)
        Debug.Assert(testFunction(New List(Of Integer) From {9,6,4,2,3,5,7,0,1}) = 8)
        Debug.Assert(testFunction(New List(Of Integer) From {0}) = 1)

        Console.WriteLine("FindMissingNumber function passed all tests")
    End Sub

    Sub Main()
        ' Testing the FindMissingNumber function
        Check(AddressOf FindMissingNumber)
    End Sub
End Module