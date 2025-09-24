function MyAtoi {
    param (
        [Parameter(Mandatory=$true)]
        [string]$s
    )

    # Remove leading whitespace
    $s = $s.TrimStart()

    # Check if string is empty after trimming
    if ($s.Length -eq 0) {
        return 0
    }

    # Determine the sign
    $sign = 1
    $i = 0

    if ($s[0] -eq '-') {
        $sign = -1
        $i++
    }
    elseif ($s[0] -eq '+') {
        $i++
    }

    # Process digits
    $result = 0
    $maxInt = [int]::MaxValue
    $minInt = [int]::MinValue

    while ($i -lt $s.Length -and [char]::IsDigit($s[$i])) {
        $digit = [int]::Parse($s[$i].ToString())

        # Check for overflow before updating result
        if ($result -gt [math]::Floor($maxInt / 10) -or 
            ($result -eq [math]::Floor($maxInt / 10) -and $digit -gt $maxInt % 10)) {
            return $maxInt
        }

        if ($result -lt [math]::Ceiling($minInt / 10) -or 
            ($result -eq [math]::Ceiling($minInt / 10) -and $digit -lt $minInt % 10)) {
            return $minInt
        }

        $result = $result * 10 + $digit * $sign
        $i++
    }

    return $result
}


# Test cases
function Check-MyAtoi() {
if ((MyAtoi "42") -ne 42) { throw "Test case 1 failed" }
if ((MyAtoi " -42") -ne -42) { throw "Test case 2 failed" }
if ((MyAtoi "4193 with words") -ne 4193) { throw "Test case 3 failed" }
if ((MyAtoi "words and 987") -ne 0) { throw "Test case 4 failed" }
}
Check-MyAtoi