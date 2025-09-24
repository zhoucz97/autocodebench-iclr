function Determine-Parity {
    param (
        [Parameter(Mandatory=$true)]
        [string[]]$numbers
    )

    $result = @()
    
    foreach ($number in $numbers) {
        # Check if the last character is a digit (0-9)
        if ($number -match '\d$') {
            $lastDigit = [int]($number[-1])
            
            if ($lastDigit % 2 -eq 0) {
                $result += "even"
            } else {
                $result += "odd"
            }
        } else {
            # Handle case where the string doesn't end with a digit (though problem says it's numbers)
            $result += "invalid"
        }
    }
    
    return $result
}


function Test-DetermineParity() {
$result1 = Determine-Parity @("1024", "5931")

if (Compare-Object $result1 @("even", "odd")) { throw "Test case 1 failed" }

$result2 = Determine-Parity @("123456789012345678901234567890", "123456789012345678901234567891")
if (Compare-Object $result2 @("even", "odd")) { throw "Test case 2 failed" }
}

Test-DetermineParity