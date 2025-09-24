Module Module1
Function CharToAscii(ByVal character As Char) As Integer
    Return Asc(character)
End Function

    Sub Main()
        ' Test the CharToAscii function
        Debug.Assert(CharToAscii("A"c) = 65)
        Debug.Assert(CharToAscii("a"c) = 97)
        Debug.Assert(CharToAscii("0"c) = 48)
        Debug.Assert(CharToAscii("$"c) = 36)
        Debug.Assert(CharToAscii("@"c) = 64)
        Debug.Assert(CharToAscii("~"c) = 126)
        Debug.Assert(CharToAscii("!"c) = 33)
    End Sub
End Module