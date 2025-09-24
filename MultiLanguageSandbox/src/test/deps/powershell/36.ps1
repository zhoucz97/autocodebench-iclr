function Get-OptimalQueueOrder {
    param (
        [Parameter(Mandatory=$true)]
        [int[]]$waitingTimes
    )

    # Create a list of objects to keep track of original indices and waiting times
    $people = @()
    for ($i = 0; $i -lt $waitingTimes.Length; $i++) {
        $people += [PSCustomObject]@{
            Index = $i + 1  # Assuming 1-based indexing as per the example
            Time = $waitingTimes[$i]
        }
    }

    # Sort the people by their waiting time in ascending order
    $sortedPeople = $people | Sort-Object -Property Time

    # Extract the original indices in the sorted order to form the optimal order
    $order = $sortedPeople | ForEach-Object { $_.Index }

    # Return the result as a custom object with the 'order' property
    return [PSCustomObject]@{
        order = $order
    }
}


function Test-OptimalQueueOrder() {
$result1 = Get-OptimalQueueOrder @(20, 10, 30)
if ((Compare-Object $result1 @(1, 0, 2))) { throw "Test case 1 failed" }

$result2 = Get-OptimalQueueOrder @(5, 3, 8)
if ((Compare-Object $result2 @(1, 0, 2))) { throw "Test case 2 failed" }

$result3 = Get-OptimalQueueOrder @(15, 5, 3, 7)
if ((Compare-Object $result3 @(2, 1, 3, 0))) { throw "Test case 3 failed" }
}

Test-OptimalQueueOrder