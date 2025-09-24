function Remove-Element {
    param (
        [Parameter(Mandatory=$true)]
        [System.Collections.Generic.List[int]]$nums,
        
        [Parameter(Mandatory=$true)]
        [int]$val
    )

    # Initialize a pointer for the position of the next non-val element
    $i = 0
    
    # Iterate through the list
    for ($j = 0; $j -lt $nums.Count; $j++) {
        if ($nums[$j] -ne $val) {
            # If current element is not val, move it to the position of i
            $nums[$i] = $nums[$j]
            $i++
        }
    }
    
    # The new length is the value of i
    return $i
}


function Check-RemoveElement() {
#if ((Remove-Element @(3, 2, 2, 3) 3) -ne 2) { throw "Test case 1 failed" }
if ((Remove-Element @(0, 1, 2, 2, 3, 0, 4, 2) 2) -ne 5) { throw "Test case 2 failed" }
if ((Remove-Element @(1, 2, 3, 4, 5) 6) -ne 5) { throw "Test case 3 failed" }
if ((Remove-Element @(1, 1, 1) 1) -ne 0) { throw "Test case 4 failed" }
if ((Remove-Element @(2, 2, 3, 3) 2) -ne 2) { throw "Test case 5 failed" }
}

Check-RemoveElement