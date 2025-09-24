Module Module1
Function MaxTeams(ByVal femalePlayers As Integer, ByVal malePlayers As Integer, ByVal internships As Integer) As Integer
    ' Calculate available players after accounting for internships
    Dim availableFemales As Integer = femalePlayers - internships
    Dim availableMales As Integer = malePlayers - internships

    ' Ensure that the number of available players is not negative
    If availableFemales < 0 Then availableFemales = 0
    If availableMales < 0 Then availableMales = 0

    ' Calculate the maximum number of teams based on the constraints
    Dim maxTeamsBasedOnMales As Integer = availableMales
    Dim maxTeamsBasedOnFemales As Integer = availableFemales \ 2

    ' The actual maximum number of teams is the minimum of the two values
    Return Math.Min(maxTeamsBasedOnMales, maxTeamsBasedOnFemales)
End Function

Sub Check(ByVal testFunction As Func(Of Integer, Integer, Integer, Integer))
Debug.Assert(testFunction(6, 3, 2) = 2)
Debug.Assert(testFunction(10, 5, 4) = 3)
Debug.Assert(testFunction(5, 3, 1) = 2)
Debug.Assert(testFunction(4, 2, 0) = 2)
Debug.Assert(testFunction(3, 2, 2) = 1)
Debug.Assert(testFunction(8, 4, 3) = 3)
Debug.Assert(testFunction(7, 3, 2) = 2)

Console.WriteLine("pass")
End Sub
Sub Main()
' Test the function
Check(AddressOf MaxTeams)
End Sub
End Module