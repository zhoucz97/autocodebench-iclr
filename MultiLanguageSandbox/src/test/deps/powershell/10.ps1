function Reverse-Integer {
    param (
        [Parameter(Mandatory=$true)]
        [int]$x
    )

    $reversed = 0
    $original = $x
    $sign = 1
    
    # Handle negative numbers
    if ($x -lt 0) {
        $sign = -1
        $x = [Math]::Abs($x)
    }
    
    while ($x -ne 0) {
        $digit = $x % 10
        $x = [Math]::Floor($x / 10)
        
        # Check for overflow before updating reversed
        if ($reversed -gt [int]::MaxValue / 10 -or 
            ($reversed -eq [int]::MaxValue / 10 -and $digit -gt [int]::MaxValue % 10)) {
            return 0
        }
        if ($reversed -lt [int]::MinValue / 10 -or 
            ($reversed -eq [int]::MinValue / 10 -and $digit -lt [int]::MinValue % 10)) {
            return 0
        }
        
        $reversed = $reversed * 10 + $digit
    }
    
    return $reversed * $sign
}


function Check-ReverseInteger() {
if ((Reverse-Integer 123) -ne 321) { throw "Test case 1 failed" }
if ((Reverse-Integer -123) -ne -321) { throw "Test case 2 failed" }
if ((Reverse-Integer 120) -ne 21) { throw "Test case 3 failed" }
if ((Reverse-Integer 0) -ne 0) { throw "Test case 4 failed" }
if ((Reverse-Integer 1) -ne 1) { throw "Test case 5 failed" }
if ((Reverse-Integer -10) -ne -1) { throw "Test case 6 failed" }

}

Check-ReverseInteger