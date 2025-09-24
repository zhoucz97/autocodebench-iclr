Module Module1
Function AsciiToChar(ByVal asciiCode As Integer) As Char
    Return Chr(asciiCode)
End Function

    Sub Main()
        ' Test the AsciiToChar function
        Debug.Assert(AsciiToChar(65) = "A")
        Debug.Assert(AsciiToChar(97) = "a")
        Debug.Assert(AsciiToChar(48) = "0")
        Debug.Assert(AsciiToChar(36) = "$")
        Debug.Assert(AsciiToChar(64) = "@")
        Debug.Assert(AsciiToChar(32) = " ")
        Debug.Assert(AsciiToChar(126) = "~")
    End Sub
End Module