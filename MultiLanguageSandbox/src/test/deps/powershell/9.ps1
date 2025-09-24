function Find-MedianOfTwoSortedArrays {
    param (
        [Parameter(Mandatory=$true)]
        [System.Collections.Generic.List[int]]$nums1,
        
        [Parameter(Mandatory=$true)]
        [System.Collections.Generic.List[int]]$nums2
    )

    # Merge the two sorted arrays
    $merged = New-Object System.Collections.Generic.List[int]
    $i = 0
    $j = 0
    
    while ($i -lt $nums1.Count -and $j -lt $nums2.Count) {
        if ($nums1[$i] -le $nums2[$j]) {
            $merged.Add($nums1[$i])
            $i++
        } else {
            $merged.Add($nums2[$j])
            $j++
        }
    }
    
    # Add remaining elements from nums1
    while ($i -lt $nums1.Count) {
        $merged.Add($nums1[$i])
        $i++
    }
    
    # Add remaining elements from nums2
    while ($j -lt $nums2.Count) {
        $merged.Add($nums2[$j])
        $j++
    }
    
    # Calculate median
    $totalLength = $merged.Count
    if ($totalLength % 2 -eq 1) {
        # Odd number of elements - return middle element
        $median = $merged[[math]::Floor($totalLength / 2)]
    } else {
        # Even number of elements - return average of two middle elements
        $mid1 = $merged[($totalLength / 2) - 1]
        $mid2 = $merged[$totalLength / 2]
        $median = ($mid1 + $mid2) / 2.0
    }
    
    return $median
}


function Check-MedianOfTwoSortedArrays() {
if ((Find-MedianOfTwoSortedArrays @([int[]](1, 3)) @([int[]](2))) -ne 2.0) { throw "Test case 1 failed" }
if ((Find-MedianOfTwoSortedArrays @([int[]](1, 2)) @([int[]](3, 4))) -ne 2.5) { throw "Test case 2 failed" }
if ((Find-MedianOfTwoSortedArrays @([int[]](1, 3, 5, 7)) @([int[]](2, 4, 6, 8))) -ne 4.5) { throw "Test case 3 failed" }
if ((Find-MedianOfTwoSortedArrays @([int[]](1)) @([int[]](1))) -ne 1.0) { throw "Test case 4 failed" }
if ((Find-MedianOfTwoSortedArrays @([int[]](1, 3, 5)) @([int[]](2))) -ne 2.5) { throw "Test case 5 failed" }
if ((Find-MedianOfTwoSortedArrays @([int[]](10, 20, 30, 40, 50)) @([int[]](5, 15, 25, 35, 45))) -ne 27.5) { throw "Test case 6 failed" }
if ((Find-MedianOfTwoSortedArrays @([int[]](1, 12, 15, 26, 38)) @([int[]](2, 13, 17, 30, 45, 50))) -ne 17.0) { throw "Test case 7 failed" }
if ((Find-MedianOfTwoSortedArrays @([int[]](1, 4, 5)) @([int[]](2, 3, 6, 7, 8))) -ne 4.5) { throw "Test case 8 failed" }
if ((Find-MedianOfTwoSortedArrays @([int[]](1, 1, 1)) @([int[]](1, 1, 1, 1))) -ne 1.0) { throw "Test case 10 failed" }
}

Check-MedianOfTwoSortedArrays