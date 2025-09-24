import Foundation

func quicksort(_ list: [Int]?) -> [Int] {
    guard let list = list, list.count > 1 else {
        return list ?? []
    }
    
    let pivot = list[0]
    let rest = Array(list.dropFirst())
    let less = rest.filter { $0 < pivot }
    let greater = rest.filter { $0 >= pivot }
    
    return quicksort(less) + [pivot] + quicksort(greater)
}

func checkQuicksort() {
    assert(quicksort([3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]) == [1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 9])
    assert(quicksort([5, 4, 3, 2, 1]) == [1, 2, 3, 4, 5])
    assert(quicksort([]) == [])
    assert(quicksort([1]) == [1])
}

checkQuicksort()
