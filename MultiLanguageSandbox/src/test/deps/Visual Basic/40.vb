Module Module1
Function ReverseArray(ByVal array As Double()) As Double()
    ' Handle empty array case
    If array Is Nothing OrElse array.Length = 0 Then
        Return New Double() {}
    End If
    
    ' Create a new array with the same length as the input
    Dim reversedArray(array.Length - 1) As Double
    
    ' Copy elements in reverse order
    For i As Integer = 0 To array.Length - 1
        reversedArray(i) = array(array.Length - 1 - i)
    Next
    
    Return reversedArray
End Function

    Sub CheckReverse(ByVal testFunction As Func(Of Double(), Double()))
        ' Test cases
        Debug.Assert(testFunction(New Double() {1.0, 2.0, 3.0}).SequenceEqual(New Double() {3.0, 2.0, 1.0}))
        Debug.Assert(testFunction(New Double() {4.5, 6.7, 8.9, 2.3}).SequenceEqual(New Double() {2.3, 8.9, 6.7, 4.5}))
        Debug.Assert(testFunction(New Double() {}).SequenceEqual(New Double() {}))
        Debug.Assert(testFunction(New Double() {7.0}).SequenceEqual(New Double() {7.0}))

        Console.WriteLine("ReverseArray function passed all tests")
    End Sub

    Sub Main()
        ' Testing the ReverseArray function
        CheckReverse(AddressOf ReverseArray)
    End Sub
End Module