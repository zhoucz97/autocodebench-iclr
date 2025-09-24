function Get-LongestUniqueSubstringLength {
    param (
        [Parameter(Mandatory=$true)]
        [string]$s
    )

    $maxLength = 0
    $start = 0
    $charIndexMap = @{}

    for ($end = 0; $end -lt $s.Length; $end++) {
        $currentChar = $s[$end]
        
        if ($charIndexMap.ContainsKey($currentChar) -and $charIndexMap[$currentChar] -ge $start) {
            $start = $charIndexMap[$currentChar] + 1
        }
        
        $charIndexMap[$currentChar] = $end
        $currentLength = $end - $start + 1
        if ($currentLength -gt $maxLength) {
            $maxLength = $currentLength
        }
    }

    return $maxLength
}


function Check-LongestUniqueSubstringLength() {
if ((Get-LongestUniqueSubstringLength "abcabcbb") -ne 3) { throw "Test case 1 failed" }
if ((Get-LongestUniqueSubstringLength "bbbbb") -ne 1) { throw "Test case 2 failed" }
if ((Get-LongestUniqueSubstringLength "pwwkew") -ne 3) { throw "Test case 3 failed" }
if ((Get-LongestUniqueSubstringLength " ") -ne 1) { throw "Test case 4 failed" }
if ((Get-LongestUniqueSubstringLength "dvdf") -ne 3) { throw "Test case 5 failed" }
if ((Get-LongestUniqueSubstringLength "anviaj") -ne 5) { throw "Test case 6 failed" }
if ((Get-LongestUniqueSubstringLength "abba") -ne 2) { throw "Test case 7 failed" }
}

Check-LongestUniqueSubstringLength