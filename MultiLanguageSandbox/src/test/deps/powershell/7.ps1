function Find-NeedleInHaystack {
    param (
        [Parameter(Mandatory=$true)]
        [string]$haystack,
        
        [Parameter(Mandatory=$true)]
        [string]$needle
    )

    # Check if needle is empty string
    if ([string]::IsNullOrEmpty($needle)) {
        return 0
    }

    # Use the IndexOf method to find the first occurrence
    $index = $haystack.IndexOf($needle)

    return $index
}


function Check-NeedleInHaystack() {
if ((Find-NeedleInHaystack "hello world" "world") -ne 6) { throw "Test case 1 failed" }
if ((Find-NeedleInHaystack "hello world" "goodbye") -ne -1) { throw "Test case 2 failed" }
if ((Find-NeedleInHaystack "openai" "ai") -ne 4) { throw "Test case 3 failed" }
if ((Find-NeedleInHaystack "chatbot" "bot") -ne 4) { throw "Test case 4 failed" }
if ((Find-NeedleInHaystack "chatbot" "hello") -ne -1) { throw "Test case 5 failed" }
if ((Find-NeedleInHaystack "abcdefg" "cd") -ne 2) { throw "Test case 6 failed" }
if ((Find-NeedleInHaystack "abcdefg" "xyz") -ne -1) { throw "Test case 7 failed" }
}

Check-NeedleInHaystack