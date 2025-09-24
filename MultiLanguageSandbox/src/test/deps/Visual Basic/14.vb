Module Module1
Function ConvertToLowercase(ByVal inputString As String) As String
    Dim result As New System.Text.StringBuilder()
    
    For Each c As Char In inputString
        If Char.IsLower(c) Then
            result.Append(Char.ToUpper(c))
        Else
            result.Append(c)
        End If
    Next
    
    Return result.ToString()
End Function

Sub Check(ByVal testFunction As Func(Of String, String))
Debug.Assert(testFunction("VisualBasic4") = "VISUALBASIC4")
Debug.Assert(testFunction("Test123!") = "TEST123!")
Debug.Assert(testFunction("helloWorld") = "HELLOWORLD")
Debug.Assert(testFunction("BUAA-4") = "BUAA-4")

Console.WriteLine("All tests passed successfully!")
End Sub

Sub Main()
' Test the function
Check(AddressOf ConvertToLowercase)
End Sub
End Module