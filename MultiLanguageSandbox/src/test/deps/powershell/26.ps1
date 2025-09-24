function Maximize-VKOccurrences {
    param (
        [Parameter(Mandatory=$true)]
        [string]$inputString
    )

    # Function to count the number of "VK" substrings in a string
    function Count-VK($str) {
        $count = 0
        for ($i = 0; $i -lt $str.Length - 1; $i++) {
            if ($str[$i] -eq 'V' -and $str[$i + 1] -eq 'K') {
                $count++
            }
        }
        return $count
    }

    $originalCount = Count-VK $inputString
    $maxCount = $originalCount

    # Iterate through each character in the string
    for ($i = 0; $i -lt $inputString.Length; $i++) {
        $currentChar = $inputString[$i]
        # Try changing to 'V' if current is not 'V'
        if ($currentChar -ne 'V') {
            $newString = $inputString.Substring(0, $i) + 'V' + $inputString.Substring($i + 1)
            $newCount = Count-VK $newString
            if ($newCount -gt $maxCount) {
                $maxCount = $newCount
            }
        }
        # Try changing to 'K' if current is not 'K'
        if ($currentChar -ne 'K') {
            $newString = $inputString.Substring(0, $i) + 'K' + $inputString.Substring($i + 1)
            $newCount = Count-VK $newString
            if ($newCount -gt $maxCount) {
                $maxCount = $newCount
            }
        }
    }

    return $maxCount
}


function Test-MaximizeVKOccurrences() {
if ((Maximize-VKOccurrences -inputString "VK") -ne 1) { throw "Test case 1 failed" }
if ((Maximize-VKOccurrences -inputString "VV") -ne 1) { throw "Test case 2 failed" }
if ((Maximize-VKOccurrences -inputString "V") -ne 0) { throw "Test case 3 failed" }
if ((Maximize-VKOccurrences -inputString "VKKKKKKKKKVVVVVVVVVK") -ne 3) { throw "Test case 4 failed" }
if ((Maximize-VKOccurrences -inputString "KVKV") -ne 1) { throw "Test case 5 failed" }
}

Test-MaximizeVKOccurrences