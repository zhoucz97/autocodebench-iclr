function FindLongestCommonPrefix {
    param (
        [Parameter(Mandatory=$true)]
        [string[]]$strs
    )

    if ($strs.Count -eq 0) {
        return ""
    }

    $prefix = $strs[0]
    
    for ($i = 1; $i -lt $strs.Count; $i++) {
        while ($strs[$i].StartsWith($prefix) -eq $false) {
            $prefix = $prefix.Substring(0, $prefix.Length - 1)
            
            if ($prefix.Length -eq 0) {
                return ""
            }
        }
    }

    return $prefix
}


function Check-FindLongestCommonPrefix() {
if ((FindLongestCommonPrefix @("flower","flow","flight")) -ne "fl") { throw "Test case 1 failed" }
if ((FindLongestCommonPrefix @("dog","racecar","car")) -ne "") { throw "Test case 2 failed" }
if ((FindLongestCommonPrefix @("interspecies","interstellar","interstate")) -ne "inters") { throw "Test case 3 failed" }
if ((FindLongestCommonPrefix @("throne","throne")) -ne "throne") { throw "Test case 4 failed" }
if ((FindLongestCommonPrefix @("a","ab")) -ne "a") { throw "Test case 5 failed" }
if ((FindLongestCommonPrefix @("abc","abcd","ab")) -ne "ab") { throw "Test case 6 failed" }
if ((FindLongestCommonPrefix @("complete","compliment","complex")) -ne "compl") { throw "Test case 7 failed" }
if ((FindLongestCommonPrefix @("nomatch","nomadic","noir")) -ne "no") { throw "Test case 8 failed" }
}

Check-FindLongestCommonPrefix