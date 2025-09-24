Module Module1
Function ReverseThreeDigitNumber(ByVal n As Integer) As Integer
    ' Extract each digit of the three-digit number
    Dim hundreds As Integer = n \ 100
    Dim tens As Integer = (n \ 10) Mod 10
    Dim units As Integer = n Mod 10
    
    ' Reverse the digits and form the new number
    Dim reversedNumber As Integer = units * 100 + tens * 10 + hundreds
    
    Return reversedNumber
End Function

    Sub Main()
        ' Test the ReverseThreeDigitNumber function
        Debug.Assert(ReverseThreeDigitNumber(358) = 853)
        Debug.Assert(ReverseThreeDigitNumber(100) = 1)
        Debug.Assert(ReverseThreeDigitNumber(250) = 52)
        Debug.Assert(ReverseThreeDigitNumber(999) = 999)
        Debug.Assert(ReverseThreeDigitNumber(123) = 321)
        Debug.Assert(ReverseThreeDigitNumber(505) = 505)
        Debug.Assert(ReverseThreeDigitNumber(210) = 12)
    End Sub
End Module