function Get-MinimumMilkCost {
    param (
        [Parameter(Mandatory=$true)]
        [int]$requiredMilk,
        
        [Parameter(Mandatory=$true)]
        [System.Collections.Generic.List[object]]$farmers
    )

    # Sort farmers by price per unit (ascending order)
    $sortedFarmers = $farmers | Sort-Object -Property price
    
    $totalCost = 0
    $remainingMilk = $requiredMilk
    
    foreach ($farmer in $sortedFarmers) {
        if ($remainingMilk -le 0) {
            break
        }
        
        $takeQuantity = [Math]::Min($farmer.quantity, $remainingMilk)
        $totalCost += $takeQuantity * $farmer.price
        $remainingMilk -= $takeQuantity
    }
    
    return $totalCost
}


function Test-MinimumMilkCost() {
$farmers1 = @([PSCustomObject]@{price=5; quantity=20}, [PSCustomObject]@{price=9; quantity=40}, [PSCustomObject]@{price=4; quantity=50})
$result1 = Get-MinimumMilkCost 100 $farmers1
Get-MinimumMilkCost 100 $farmers1
if ($result1 -ne 570) { throw "Test case 1 failed: Expected 580, got $result1" }

$farmers2 = @([PSCustomObject]@{price=10; quantity=30}, [PSCustomObject]@{price=7; quantity=60}, [PSCustomObject]@{price=2; quantity=40})
$result2 = Get-MinimumMilkCost 50 $farmers2
if ($result2 -ne 150) { throw "Test case 2 failed: Expected 140, got $result2" }

$farmers3 = @([PSCustomObject]@{price=8; quantity=25}, [PSCustomObject]@{price=5; quantity=55}, [PSCustomObject]@{price=6; quantity=20})
$result3 = Get-MinimumMilkCost 75 $farmers3
if ($result3 -ne 395) { throw "Test case 3 failed: Expected 430, got $result3" }
}

Test-MinimumMilkCost