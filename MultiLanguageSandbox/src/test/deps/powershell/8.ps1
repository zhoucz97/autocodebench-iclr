function Find-ConcatenatedSubstrings {
    param (
        [Parameter(Mandatory=$true)]
        [string]$s,
        
        [Parameter(Mandatory=$true)]
        [string[]]$words
    )

    # Check for empty input
    if ($words.Count -eq 0) {
        return @()
    }

    $wordLength = $words[0].Length
    $totalWords = $words.Count
    $totalLength = $wordLength * $totalWords
    $result = @()

    # Create a frequency dictionary for the words
    $wordCount = @{}
    foreach ($word in $words) {
        if ($wordCount.ContainsKey($word)) {
            $wordCount[$word]++
        } else {
            $wordCount[$word] = 1
        }
    }

    # Iterate through the string
    for ($i = 0; $i -le $s.Length - $totalLength; $i++) {
        $seen = @{}
        $j = 0
        while ($j -lt $totalWords) {
            $start = $i + $j * $wordLength
            $currentWord = $s.Substring($start, $wordLength)
            
            # Check if the current word is in the word list
            if (-not $wordCount.ContainsKey($currentWord)) {
                break
            }
            
            # Update the seen count
            if ($seen.ContainsKey($currentWord)) {
                $seen[$currentWord]++
            } else {
                $seen[$currentWord] = 1
            }
            
            # Check if we've seen too many of this word
            if ($seen[$currentWord] -gt $wordCount[$currentWord]) {
                break
            }
            
            $j++
        }
        
        # If we processed all words successfully
        if ($j -eq $totalWords) {
            $result += $i
        }
    }

    return $result
}


function Check-ConcatenatedSubstrings() {
$result = Find-ConcatenatedSubstrings "barfoothefoobarman" @("foo","bar")
if (($result -join ',') -ne '0,9') { throw "Test case 1 failed" }

$result = Find-ConcatenatedSubstrings "wordgoodgoodgoodbestword" @("word","good","best","word")
if (($result -join ',') -ne '') { throw "Test case 2 failed" }

$result = Find-ConcatenatedSubstrings "abcabcabc" @("abc","abc")
if (($result -join ',') -ne '0,3') { throw "Test case 3 failed" }

$result = Find-ConcatenatedSubstrings "abcd" @("ab","cd")
if (($result -join ',') -ne '0') { throw "Test case 4 failed" }

$result = Find-ConcatenatedSubstrings "abcd" @("cd","ab")
if (($result -join ',') -ne '0') { throw "Test case 5 failed" }

$result = Find-ConcatenatedSubstrings "abababab" @("ab","ba")
if (($result -join ',') -ne '') { throw "Test case 6 failed" }

$result = Find-ConcatenatedSubstrings "abcdef" @("gh","ij")
if (($result -join ',') -ne '') { throw "Test case 7 failed" }
}

Check-ConcatenatedSubstrings