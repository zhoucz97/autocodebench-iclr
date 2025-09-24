function Remove-DuplicatesFromSortedArray {
    param (
        [Parameter(Mandatory=$true)]
        [System.Collections.Generic.List[int]]$nums
    )

    if ($nums.Count -eq 0) {
        return 0
    }

    $i = 0
    for ($j = 1; $j -lt $nums.Count; $j++) {
        if ($nums[$j] -ne $nums[$i]) {
            $i++
            $nums[$i] = $nums[$j]
        }
    }

    return $i + 1
}


function Check-RemoveDuplicatesFromSortedArray() {
if ((Remove-DuplicatesFromSortedArray @(1, 1, 2)) -ne 2) { throw "Test case 1 failed" }
if ((Remove-DuplicatesFromSortedArray @(0,0,1,1,1,2,2,3,3,4)) -ne 5) { throw "Test case 2 failed" }
if ((Remove-DuplicatesFromSortedArray @(1, 2, 3)) -ne 3) { throw "Test case 3 failed" }
if ((Remove-DuplicatesFromSortedArray @(2, 2, 2)) -ne 1) { throw "Test case 4 failed" }
if ((Remove-DuplicatesFromSortedArray @(1)) -ne 1) { throw "Test case 5 failed" }
if ((Remove-DuplicatesFromSortedArray @(1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 4)) -ne 4) { throw "Test case 6 failed" }
}

Check-RemoveDuplicatesFromSortedArray