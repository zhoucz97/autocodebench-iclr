function Get-LongestPalindromicSubstring {
    param (
        [Parameter(Mandatory=$true)]
        [string]$s
    )

    # Preprocess the string to handle even length palindromes
    $processed = "^#" + ($s -replace '(.*)', '#$1#') + "#$"
    $n = $processed.Length
    $p = New-Object int[] $n
    $center = 0
    $right = 0

    for ($i = 1; $i -lt $n - 1; $i++) {
        if ($i -lt $right) {
            $mirror = 2 * $center - $i
            $p[$i] = [Math]::Min($right - $i, $p[$mirror])
        }

        # Attempt to expand palindrome centered at i
        while ($processed[$i + $p[$i] + 1] -eq $processed[$i - $p[$i] - 1]) {
            $p[$i]++
        }

        # If palindrome centered at i expands past right,
        # adjust center based on expanded palindrome.
        if ($i + $p[$i] -gt $right) {
            $center = $i
            $right = $i + $p[$i]
        }
    }

    # Find the maximum element in P.
    $maxLen = 0
    $centerIndex = 0
    for ($i = 0; $i -lt $n; $i++) {
        if ($p[$i] -gt $maxLen) {
            $maxLen = $p[$i]
            $centerIndex = $i
        }
    }

    # Extract the longest palindrome from processed string
    $start = ($centerIndex - $maxLen) / 2
    $end = $start + $maxLen - 1
    return $s.Substring($start, $end - $start + 1)
}


function Check-LongestPalindromicSubstring() {
$result1 = Get-LongestPalindromicSubstring "babad"
if (-not ($result1 -eq "bab" -or $result1 -eq "aba")) { throw "Test case 1 failed" }
if ((Get-LongestPalindromicSubstring "cbbd") -ne "bb") { throw "Test case 2 failed" }
if ((Get-LongestPalindromicSubstring "a") -ne "a") { throw "Test case 3 failed" }
if ((Get-LongestPalindromicSubstring "racecar") -ne "racecar") { throw "Test case 4 failed" }
if ((Get-LongestPalindromicSubstring "madam") -ne "madam") { throw "Test case 5 failed" }
if ((Get-LongestPalindromicSubstring "abcdcba") -ne "abcdcba") { throw "Test case 6 failed" }
}

Check-LongestPalindromicSubstring