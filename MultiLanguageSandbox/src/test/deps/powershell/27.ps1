function Calculate-FinalGrade {
    param (
        [Parameter(Mandatory=$true)]
        [int]$homeworkScore,
        
        [Parameter(Mandatory=$true)]
        [int]$quizScore,
        
        [Parameter(Mandatory=$true)]
        [int]$finalExamScore
    )
    
    # Calculate weighted scores
    $homeworkWeighted = $homeworkScore * 0.20
    $quizWeighted = $quizScore * 0.30
    $finalExamWeighted = $finalExamScore * 0.50
    
    # Sum the weighted scores to get the final grade
    $finalGrade = $homeworkWeighted + $quizWeighted + $finalExamWeighted
    
    # Return the final grade (rounded to nearest integer if needed)
    return [math]::Round($finalGrade)
}


function Test-CalculateFinalGrade() {
if ((Calculate-FinalGrade -homeworkScore 100 -quizScore 100 -finalExamScore 80) -ne 90) { throw "Test case 1 failed" }
if ((Calculate-FinalGrade -homeworkScore 60 -quizScore 90 -finalExamScore 80) -ne 79) { throw "Test case 2 failed" }
}

Test-CalculateFinalGrade