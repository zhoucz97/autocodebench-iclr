Module Module1
Function CountLeapYears(ByVal startYear As Integer, ByVal endYear As Integer) As Integer
    Dim count As Integer = 0
    
    For year As Integer = startYear To endYear
        ' Check if the year is a leap year
        If (year Mod 4 = 0 And year Mod 100 <> 0) Or (year Mod 400 = 0) Then
            count += 1
        End If
    Next
    
    Return count
End Function

Sub Check(ByVal testFunction As Func(Of Integer, Integer, Integer))
Debug.Assert(testFunction(2000, 2020) = 6) ' Includes 2000, 2004, 2008, 2012, 2016, 2020
Debug.Assert(testFunction(1900, 2000) = 25) ' Includes 1904, 1908, ..., 1996, 2000
Debug.Assert(testFunction(1980, 1990) = 3) ' Includes 1980, 1984, 1988
Debug.Assert(testFunction(1800, 1900) = 24) ' Includes 1804, 1808, ..., 1896
Debug.Assert(testFunction(1600, 1700) = 25) ' Includes 1600, 1604, ..., 1696

Console.WriteLine("pass")
End Sub

Sub Main()
' Test the function
Check(AddressOf CountLeapYears)
End Sub
End Module