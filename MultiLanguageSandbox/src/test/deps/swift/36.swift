import Foundation

func findShortestDistanceAmongPoints(_ points: [(Double, Double)]) -> Double {
    guard points.count >= 2 else {
        return 0.0 // or handle error as needed
    }
    
    var shortestDistance = Double.infinity
    
    for i in 0..<points.count {
        for j in i+1..<points.count {
            let (x1, y1) = points[i]
            let (x2, y2) = points[j]
            let distance = sqrt(pow(x2 - x1, 2) + pow(y2 - y1, 2))
            if distance < shortestDistance {
                shortestDistance = distance
            }
        }
    }
    
    return round(shortestDistance * 10000) / 10000
}


func check(_ findShortestDistanceAmongPoints: ([(Double, Double)]) -> Double) {
assert(findShortestDistanceAmongPoints([(1.0, 1.0), (1.0, 2.0), (2.0, 2.0)]) == 1.0000)
assert(findShortestDistanceAmongPoints([(0.0, 0.0), (3.0, 4.0), (1.0, 1.0)]) == 1.4142)
assert(findShortestDistanceAmongPoints([(0.0, 0.0), (5.0, 5.0), (1.0, 6.0), (2.0, 1.0)]) == 2.2361)
assert(findShortestDistanceAmongPoints([(2.0, 3.0), (4.0, 4.0)]) == 2.2361)
assert(findShortestDistanceAmongPoints([(1.5, 1.5), (3.5, 3.5), (5.5, 5.5)]) == 2.8284)
}

check(findShortestDistanceAmongPoints)