Module Module1
Function FormatPolynomial(ByVal degree As Integer, ByVal coefficients As List(Of Integer)) As String
    Dim result As New System.Text.StringBuilder()
    
    ' Check if the polynomial is empty (all coefficients are zero)
    Dim allZero As Boolean = True
    For Each coeff In coefficients
        If coeff <> 0 Then
            allZero = False
            Exit For
        Next
    If allZero Then Return "0"
    
    ' Process each term
    For i As Integer = 0 To degree
        Dim currentDegree As Integer = degree - i
        Dim coeff As Integer = coefficients(i)
        
        ' Skip zero coefficients
        If coeff = 0 Then Continue For
        
        ' Handle the first term (leading term)
        If result.Length = 0 Then
            ' No sign for positive leading term unless it's negative
            If coeff < 0 Then
                result.Append("-")
                coeff = Math.Abs(coeff)
            End If
        Else
            ' Add + or - for subsequent terms
            If coeff > 0 Then
                result.Append("+")
            Else
                result.Append("-")
                coeff = Math.Abs(coeff)
            End If
        End If
        
        ' Handle coefficient value (skip 1 unless it's the constant term)
        If coeff <> 1 OrElse currentDegree = 0 Then
            result.Append(coeff.ToString())
        End If
        
        ' Handle variable part
        If currentDegree > 0 Then
            result.Append("x")
            If currentDegree > 1 Then
                result.Append("^" & currentDegree.ToString())
            End If
        End If
    Next
    
    Return result.ToString()
End Function

Sub Check(ByVal testFunction As Func(Of Integer, List(Of Integer), String))
Debug.Assert(testFunction(2, New List(Of Integer) From {1, -2, 1}) = "x^2-2x+1")
Debug.Assert(testFunction(3, New List(Of Integer) From {-3, 0, 4, -2}) = "-3x^3+4x-2")
Debug.Assert(testFunction(4, New List(Of Integer) From {0, -1, 1, 0, -3}) = "-x^3+x^2-3")
Debug.Assert(testFunction(1, New List(Of Integer) From {3, -2}) = "3x-2")
Debug.Assert(testFunction(0, New List(Of Integer) From {5}) = "5")

Console.WriteLine("All test cases passed")
End Sub
Sub Main()
Check(AddressOf FormatPolynomial)
End Sub
End Module