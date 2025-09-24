function Get-MaxDaysTogether {
    param (
        [Parameter(Mandatory=$true)]
        [string[]]$convenientLocations,
        
        [Parameter(Mandatory=$true)]
        [string[]]$herLocations
    )
    
    # Initialize a counter for matching days
    $matchingDays = 0
    
    # Iterate through each day's location
    for ($i = 0; $i -lt $herLocations.Count; $i++) {
        # Check if the current location is in the convenient locations list
        if ($convenientLocations -contains $herLocations[$i]) {
            $matchingDays++
        }
    }
    
    return $matchingDays
}


function Test-MaxDaysTogether() {
if ((Get-MaxDaysTogether -convenientLocations @("WC") -herLocations @("CLASS", "WC")) -ne 1) { throw "Test case 1 failed" }
if ((Get-MaxDaysTogether -convenientLocations @("Park", "Mall") -herLocations @("Park", "School", "Mall", "Home")) -ne 2) { throw "Test case 2 failed" }
if ((Get-MaxDaysTogether -convenientLocations @("Library", "Cafe") -herLocations @("Home", "School", "Gym")) -ne 0) { throw "Test case 3 failed" }
}

Test-MaxDaysTogether