function Count-GoodIntegers {
    param (
        [Parameter(Mandatory=$true)]
        [string]$num1,
        
        [Parameter(Mandatory=$true)]
        [string]$num2,
        
        [Parameter(Mandatory=$true)]
        [int]$max_sum,
        
        [Parameter(Mandatory=$true)]
        [int]$min_sum
    )

    # Helper function to calculate digit sum
    function Get-DigitSum {
        param([string]$number)
        $sum = 0
        foreach ($c in $number.ToCharArray()) {
            $sum += [int][char]$c - [int][char]'0'
        }
        return $sum
    }

    # Convert strings to numbers for comparison
    $lower = [bigint]::Parse($num1)
    $upper = [bigint]::Parse($num2)
    
    $count = 0
    $mod = 1000000007
    
    # Iterate through the range
    for ($x = $lower; $x -le $upper; $x++) {
        $digitSum = Get-DigitSum($x.ToString())
        if ($digitSum -ge $min_sum -and $digitSum -le $max_sum) {
            $count++
        }
    }
    
    return $count % $mod
}


function Check-GoodIntegers() {
if ((Count-GoodIntegers "25" "30" 7 5) -ne 1) { throw "Test case 1 failed" }
if ((Count-GoodIntegers "1" "12" 8 1) -ne 11) { throw "Test case 2 failed" }
if ((Count-GoodIntegers "1" "5" 5 1) -ne 5) { throw "Test case 3 failed" }
}

Check-GoodIntegers