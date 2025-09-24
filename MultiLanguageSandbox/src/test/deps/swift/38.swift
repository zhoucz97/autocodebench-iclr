func latestDepartureTime(distance: Int, speed: Int) -> String {
    let totalMinutesNeeded = (distance / speed) + 10
    let schoolTimeInMinutes = 8 * 60 // 8:00 AM is 480 minutes since midnight
    let departureTimeInMinutes = schoolTimeInMinutes - totalMinutesNeeded
    
    let hours = departureTimeInMinutes / 60
    let minutes = departureTimeInMinutes % 60
    
    return String(format: "%02d:%02d", hours, minutes)
}


func check(_ latestDepartureTime: (Int, Int) -> String) {
assert(latestDepartureTime(100, 10) == "07:40")
assert(latestDepartureTime(50, 25) == "07:48")
assert(latestDepartureTime(120, 60) == "07:48")
assert(latestDepartureTime(80, 20) == "07:46")
assert(latestDepartureTime(200, 40) == "07:45")
assert(latestDepartureTime(90, 30) == "07:47")
}

check(latestDepartureTime)